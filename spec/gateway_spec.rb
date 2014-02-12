require 'blather'
require 'spec_helper'
require 'libertree/client'

describe Libertree::Server::Gateway do
  LSR = Libertree::Server::Responder

  before :each do
    @client = LSR.connection
  end

  it 'advertises Libertree gateway' do
    msg = Blather::Stanza::Iq::DiscoInfo.new
    ns = msg.class.registered_ns

    @client.should_receive(:write) do |stanza|
      # upstream bug: stanza.identities and stanza.features always
      # returns an empty array
      stanza.xpath('.//ns:identity[@name="Libertree Gateway" and @type="libertree" and @category="gateway"]',
                   :ns => ns).should_not be_empty

      features = stanza.xpath('.//ns:feature/@var', :ns => ns).map(&:value)
      features.should match_array([ 'http://jabber.org/protocol/disco#info',
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
      msg.from = "tester@test"
      msg.add_child Nokogiri::XML.fragment("<query xmlns='jabber:iq:register'/>")
      ns = 'jabber:iq:register'

      @client.should_receive(:write) do |stanza|
        stanza.xpath('.//ns:instructions', :ns => ns).should_not be_empty
        stanza.xpath('.//ns:username',     :ns => ns).should_not be_empty
        stanza.xpath('.//ns:password',     :ns => ns).should_not be_empty
      end
      @client.handle_data msg
    end

    it 'rejects a register attempt given an unknown username' do
      ns = 'jabber:iq:register'

      msg = Blather::Stanza::Iq.new
      msg.type = :set
      msg.add_child Nokogiri::XML::Builder.new { |xml|
        xml.query('xmlns' => ns) {
          xml.username("unknown")
          xml.password("whatever")
        }
      }.doc.root

      @client.should_receive(:write) do |stanza|
        stanza.xpath('.//error').should_not be_empty
      end
      @client.handle_data msg
    end

    it 'rejects a register attempt given a wrong password' do
      ns = 'jabber:iq:register'

      msg = Blather::Stanza::Iq.new
      msg.type = :set
      msg.add_child Nokogiri::XML::Builder.new { |xml|
        xml.query('xmlns' => ns) {
          xml.username("username")
          xml.password("wrong")
        }
      }.doc.root

      @client.should_receive(:write) do |stanza|
        stanza.xpath('.//error').should_not be_empty
      end
      @client.handle_data msg
    end

    it 'registers the jid when the credentials are okay' do
      ns = 'jabber:iq:register'

      jid = "tester@test"
      msg = Blather::Stanza::Iq.new
      msg.from = jid
      msg.type = :set
      msg.add_child Nokogiri::XML::Builder.new { |xml|
        xml.query('xmlns' => ns) {
          xml.username("username")
          xml.password("1234")
        }
      }.doc.root

      account = Libertree::Model::Account[ username: "username" ]
      account.should_not be_nil
      account.gateway_jid.should be_nil
      @client.should_receive(:write) do |stanza|
        stanza.xpath('.//error').should be_empty
      end
      @client.handle_data msg
      account = Libertree::Model::Account[ username: "username" ]
      account.gateway_jid.should eq(jid)
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
      msg.from = @jid
      msg.add_child Nokogiri::XML.fragment("<query xmlns='jabber:iq:register'/>")
      ns = 'jabber:iq:register'

      @client.should_receive(:write) do |stanza|
        stanza.xpath('.//ns:registered', :ns => ns).should_not be_empty
        stanza.xpath('.//ns:username',   :ns => ns).should_not be_empty
        stanza.xpath('.//ns:password',   :ns => ns).should_not be_empty

        stanza.xpath('.//ns:username',   :ns => ns).text.should eq(@account.username)
        stanza.xpath('.//ns:password',   :ns => ns).text.should eq(@account.password)
      end
      @client.handle_data msg
    end
  end

end
