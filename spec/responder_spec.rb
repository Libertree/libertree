require 'blather'
require 'spec_helper'
require 'libertree/client'

describe Libertree::Server::Responder do
  helper_class = Class.new
  helper_class.class_eval {
    include Libertree::XML::Helper
    include LSR::Helper
  }
  helper = helper_class.new

  before :each do
    @remote_tree = double
    @remote_tree.stub :id
    @client = LSR.connection
    @client.stub :write

    # requester using client library
    Libertree::Client.any_instance.stub(:connect)
    key = OpenSSL::PKey::RSA.new(512)
    @public_key = key.public_key.to_pem
    @contact = "admin@localhost"
    @domain = "localhost"
    @server_name = "server"

    @requester = Libertree::Client.new({ private_key: key,
                                         contact: @contact,
                                         domain: @domain,
                                         frontend_url_base: 'localhost' })

    @requester.instance_variable_set(:@contact, @contact)
    @requester.instance_variable_set(:@domain, @domain)
    @requester.instance_variable_set(:@server_name, @server_name)
  end

  it 'ignores unsupported iq set stanzas' do
    msg = Blather::Stanza::Iq.new :set
    LSR.should_not_receive(:respond)
    @client.send :call_handler_for, :iq, msg
  end

  it 'responds to any other unsupported stanza type with "UNKNOWN COMMAND"' do
    expected_response = LSR.error code: 'UNKNOWN COMMAND'

    target = "to@receiver.localhost.localdomain"
    msgs = [ Blather::Stanza::Message.new(target, 'body', :groupchat),
             Blather::Stanza::Message.new(target, 'body', :headline),
             Blather::Stanza::Message.new(target, 'body', :normal),
             Blather::Stanza::Message.new(target, 'body', :error)
           ]

    msgs.each do |msg|
      LSR.should_receive(:respond) do |args|
        args[:to].should eq msg
        args[:with].to_s.should eq expected_response.to_s
      end

      @client.send :call_handler_for, :message, msg
    end
  end


  context "when the requester is a member of one of the receiver's forests" do
    include_context 'requester in a forest'

    it 'responds with "MISSING PARAMETER" when a handler throws MissingParameterError' do
      msg = helper.build_stanza("localhost.localdomain", "<post><id>10</id></post>")
      msg.from = @requester.domain
      expected_response = LSR.error({ :code => 'MISSING PARAMETER',
                                      :text => 'username'
                                    })
      
      LSR.should_receive(:respond) do |args|
        args[:to].should eq msg
        args[:with].to_s.should eq expected_response.to_s
      end
      
      # handler throws :halt to prevent falling through to the catch-all handler
      catch(:halt) { @client.send :call_handler_for, :iq, msg }
    end

    it 'responds with "NOT FOUND" when a handler throws NotFoundError' do
      xml =<<XML
<comment>
  <id>999</id>
  <username>nosuchusername</username>
  <origin>WHATEVER</origin>
  <post_id>1234</post_id>
  <text>A test comment.</text>
</comment>
XML

      subject.instance_variable_set(:@remote_tree, @remote_tree)
      
      msg = helper.build_stanza("localhost.localdomain", xml)
      msg.from = @requester.domain
      expected_response = LSR.error({ :code => 'NOT FOUND',
                                      :text => 'Unrecognized member username: "nosuchusername"'})
      
      LSR.should_receive(:respond) do |args|
        args[:to].should eq msg
        args[:with].to_s.should eq expected_response.to_s
      end
      
      # handler throws :halt to prevent falling through to the catch-all handler
      catch(:halt) { @client.send :call_handler_for, :iq, msg }
    end
    
    it 'calls the correct handler for all valid iq commands' do
      LSR::VALID_COMMANDS.each do |command|
        stanza = helper.build_stanza("localhost.localdomain", "<#{command}/>")
        stanza.from = @requester.domain
        LSR.should_receive "rsp_#{command.gsub('-', '_')}".to_sym
        catch(:halt) { @client.send :call_handler_for, :iq, stanza }
      end
    end

    it 'calls the correct handler for chat messages' do
      stanza = Blather::Stanza::Message.new("localhost.localdomain", 'text', :chat)
      stanza.from = @requester.domain
      LSR.should_receive :rsp_chat
      catch(:halt) { @client.send :call_handler_for, :message, stanza }
    end
  end

  context "when the requester is not a member of any of the receiver's forests" do
    include_context 'requester not in any forest'

    it 'responds with UNRECOGNIZED SERVER' do
      (LSR::VALID_COMMANDS - ['forest', 'introduce']).each do |command|
        stanza = helper.build_stanza("localhost.localdomain", "<#{command}/>")
        stanza.from = @requester.domain

        err = LSR.error code: 'UNRECOGNIZED SERVER'

        LSR.should_receive(:respond) do |args|
          args[:with].to_s.should eq err.to_s
        end
        catch(:halt) { @client.send :call_handler_for, :iq, stanza }
      end
    end

    it 'does not respond with UNRECOGNIZED SERVER to "forest" commands' do
      stanza = helper.build_stanza("localhost.localdomain", '<forest/>')
      stanza.from = @requester.domain
      err = LSR.error code: 'UNRECOGNIZED SERVER'

      LSR.should_receive(:respond) do |args|
        args[:with].to_s.should_not eq err.to_s
      end
      # handler throws :halt to prevent falling through to the catch-all handler
      catch(:halt) { @client.send :call_handler_for, :iq, stanza }
    end
  end

  describe 'process' do
    it 'converts commands with dashes to method names with underscores' do
      xml = Nokogiri::XML.fragment @requester.req_post_like_delete(10)
      hash = helper.xml_to_hash(xml).values.first
      LSR.should_receive(:rsp_post_like_delete).with(hash)
      LSR.process("post-like-delete", xml)
    end
  end
end
