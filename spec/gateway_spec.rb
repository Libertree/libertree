require 'blather'
require 'spec_helper'
require 'libertree/client'

describe Libertree::Server::Gateway do
  LSR = Libertree::Server::Responder

  before :each do
    @client = LSR.connection
    @gateway = "gateway.liber.tree"
  end

  it 'advertises Libertree gateway' do
    msg = Blather::Stanza::Iq::DiscoInfo.new
    msg.to = @gateway
    ns = msg.class.registered_ns

    expect( @client ).to receive(:write) do |stanza|
      # upstream bug: stanza.identities and stanza.features always
      # returns an empty array
      expect( stanza.xpath('.//ns:identity[@name="Libertree Gateway" and @type="libertree" and @category="gateway"]',
                           :ns => ns) ).not_to be_empty

      features = stanza.xpath('.//ns:feature/@var', :ns => ns).map(&:value)
      expect( features ).to match_array([ 'http://jabber.org/protocol/disco#info',
                                          'jabber:iq:register' ])
    end
    @client.handle_data msg
  end

  context "when the sender's jid is not yet registered" do
    before :each do
      Libertree::DB.dbh.execute 'TRUNCATE accounts CASCADE'
      @account = Libertree::Model::Account.create({
        username: "username",
        password_encrypted: BCrypt::Password.create("1234")
      })
    end

    it 'requests credentials when a jabber:iq:register query is received' do
      msg = Blather::Stanza::Iq.new
      msg.to = @gateway
      msg.from = "tester@test"
      msg.add_child Nokogiri::XML.fragment("<query xmlns='jabber:iq:register'/>")
      ns = 'jabber:iq:register'

      expect( @client ).to receive(:write) do |stanza|
        expect( stanza.xpath('.//ns:instructions', :ns => ns) ).not_to be_empty
        expect( stanza.xpath('.//ns:username',     :ns => ns) ).not_to be_empty
        expect( stanza.xpath('.//ns:password',     :ns => ns) ).not_to be_empty
      end
      @client.handle_data msg
    end

    it 'rejects a register attempt given an unknown username' do
      ns = 'jabber:iq:register'

      msg = Blather::Stanza::Iq.new
      msg.to = @gateway
      msg.type = :set
      msg.add_child Nokogiri::XML::Builder.new { |xml|
        xml.query('xmlns' => ns) {
          xml.username("unknown")
          xml.password("whatever")
        }
      }.doc.root

      expect( @client ).to receive(:write) do |stanza|
        expect( stanza.xpath('.//error') ).not_to be_empty
      end
      @client.handle_data msg
    end

    it 'rejects a register attempt given a wrong password' do
      ns = 'jabber:iq:register'

      msg = Blather::Stanza::Iq.new
      msg.to = @gateway
      msg.type = :set
      msg.add_child Nokogiri::XML::Builder.new { |xml|
        xml.query('xmlns' => ns) {
          xml.username("username")
          xml.password("wrong")
        }
      }.doc.root

      expect( @client ).to receive(:write) do |stanza|
        expect( stanza.xpath('.//error') ).not_to be_empty
      end
      @client.handle_data msg
    end

    it 'registers the jid when the credentials are okay' do
      ns = 'jabber:iq:register'

      jid = "tester@test"
      msg = Blather::Stanza::Iq.new
      msg.to = @gateway
      msg.from = jid
      msg.type = :set
      msg.add_child Nokogiri::XML::Builder.new { |xml|
        xml.query('xmlns' => ns) {
          xml.username("username")
          xml.password("1234")
        }
      }.doc.root

      account = Libertree::Model::Account[ username: "username" ]
      expect( account ).not_to be_nil
      expect( account.gateway_jid ).to be_nil

      # server acknowledges stanza
      expect( @client ).to receive(:write) do |stanza|
        expect( stanza.xpath('.//error') ).to be_empty
      end

      # gateway subscribes to user's presence
      expect( @client ).to receive(:write) do |stanza|
        expect( stanza.type ).to eq(:subscribe)
      end

      @client.handle_data msg
      account = Libertree::Model::Account[ username: "username" ]
      expect( account.gateway_jid ).to eq(jid)
    end

    it 'rejects subscriptions to the gateway' do
      p = Blather::Stanza::Presence::Subscription.new(@gateway, :subscribe)
      p.from = "tester@test"

      expect( @client ).to receive(:write) do |stanza|
        expect( stanza.type ).to eq(:unsubscribed)
      end

      @client.handle_data p
    end
  end

  context "when the sender's jid is registered with an account" do
    before :each do
      Libertree::DB.dbh.execute 'TRUNCATE accounts CASCADE'
      @jid = "tester@test.net"
      @account = Libertree::Model::Account.create({
        username: "username",
        password_encrypted: BCrypt::Password.create("1234")
      })
      @account.gateway_jid = @jid
    end

    it 'responds with the user record upon receiving an empty jabber:iq:register query' do
      msg = Blather::Stanza::Iq.new
      msg.to = @gateway
      msg.from = @jid
      msg.add_child Nokogiri::XML.fragment("<query xmlns='jabber:iq:register'/>")
      ns = 'jabber:iq:register'

      expect( @client ).to receive(:write) do |stanza|
        expect( stanza.xpath('.//ns:registered', :ns => ns) ).not_to be_empty
        expect( stanza.xpath('.//ns:username',   :ns => ns) ).not_to be_empty
        expect( stanza.xpath('.//ns:password',   :ns => ns) ).not_to be_empty

        expect( stanza.xpath('.//ns:username',   :ns => ns).text ).to eq(@account.username)
        expect( stanza.xpath('.//ns:password',   :ns => ns).text ).to eq(@account.password)
      end
      @client.handle_data msg
    end

    it 'unregisters the jid upon receiving a jabber:iq:register set query with "remove" tag' do
      ns = 'jabber:iq:register'

      msg = Blather::Stanza::Iq.new
      msg.type = :set
      msg.to = @gateway
      msg.from = @jid
      msg.add_child Nokogiri::XML::Builder.new { |xml|
        xml.query('xmlns' => ns) {
          xml.remove
        }
      }.doc.root

      username = @account.username

      expect( @client ).to receive(:write) do |stanza|
        expect( stanza.xpath('.//error', :ns => ns) ).to be_empty
        account = Libertree::Model::Account[ username: username ]
        expect( account.gateway_jid ).to be_nil
      end

      expect( @client ).to receive(:write) do |stanza|
        expect( stanza.type ).to eq(:unsubscribe)
      end
      expect( @client ).to receive(:write) do |stanza|
        expect( stanza.type ).to eq(:unsubscribed)
      end
      expect( @client ).to receive(:write) do |stanza|
        expect( stanza.type ).to eq(:unavailable)
      end

      @client.handle_data msg
    end

    it 'approves subscriptions to the gateway' do
      p = Blather::Stanza::Presence::Subscription.new(@gateway, :subscribe)
      p.from = @jid

      expect( @client ).to receive(:write) do |stanza|
        expect( stanza.type ).to eq(:subscribed)
      end

      @client.handle_data p
    end

    it 'responds to "log out" stanzas' do
      p = Blather::Stanza::Presence.new(@gateway)
      p.type = :unavailable
      p.from = @jid

      expect( @client ).to receive(:write) do |stanza|
        expect( stanza.type ).to eq(:unavailable)
      end

      @client.handle_data p
    end
  end

end
