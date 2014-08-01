require 'blather'
require 'spec_helper'
require 'libertree/client'

describe Libertree::Server::Disco do
  Disco = Libertree::Server::Disco

  before :each do
    @client = LSR.connection
    @client.stub :write
    @jid = 'disco.liber.tree'
  end

  describe 'register_identity' do
    it 'appends an identity to the list of identities' do
      msg = Blather::Stanza::Iq::DiscoInfo.new
      msg.to = @jid
      ns = msg.class.registered_ns

      Disco.register_identity({ :name => 'Test Identity',
                                :type => 'service',
                                :category => 'whatever' })

      expect( @client ).to receive(:write) do |stanza|
        # upstream bug: stanza.identities and stanza.features always
        # returns an empty array
        expect( stanza.xpath('.//ns:identity[@name="Test Identity" and @type="service" and @category="whatever"]',
                             :ns => ns) ).not_to be_empty
      end
      @client.handle_data msg
    end
  end

  describe 'register_dynamic_node_info' do
    it 'supports dynamic identities and features' do
      msg = Blather::Stanza::Iq::DiscoInfo.new
      msg.to = @jid
      ns = msg.class.registered_ns

      identity_rule = lambda do |node_path|
        if node_path
          [{
             :name => 'with node path',
             :type => 'test',
             :category => 'test'
           },
           ['feature-with-node-path']]
        else
          [{
             :name => 'without node path',
             :type => 'test',
             :category => 'test'
           },
           ['feature-without-node-path']]
        end
      end
      Disco.register_dynamic_node_info(identity_rule)

      expect( @client ).to receive(:write) do |stanza|
        # upstream bug: stanza.identities and stanza.features always
        # returns an empty array
        expect( stanza.xpath('.//ns:identity[@name="without node path" and @type="test" and @category="test"]',
                             :ns => ns) ).not_to be_empty
        features = stanza.xpath('.//ns:feature/@var', :ns => ns).map(&:value)
        expect( features ).to include('feature-without-node-path')
       end
      @client.handle_data msg

      msg.node = 'a-pubsub-node-path'
      expect( @client ).to receive(:write) do |stanza|
        # upstream bug: stanza.identities and stanza.features always
        # returns an empty array
        expect( stanza.xpath('.//ns:identity[@name="with node path" and @type="test" and @category="test"]',
                             :ns => ns) ).not_to be_empty
        features = stanza.xpath('.//ns:feature/@var', :ns => ns).map(&:value)
        expect( features ).to include('feature-with-node-path')
      end
      @client.handle_data msg
    end

    it 'will not append empty identities' do
      before = Disco.class_variable_get(:@@identities)
      Disco.register_identity(lambda {|n| nil}, :rule)
      after = Disco.class_variable_get(:@@identities)

      expect( after ).to eq(before)
    end
  end

  describe 'register_feature' do
    it 'adds a feature to the list of features' do
      msg = Blather::Stanza::Iq::DiscoInfo.new
      msg.to = @jid
      ns = msg.class.registered_ns

      Disco.register_feature 'my-own-feature'

      expect( @client ).to receive(:write) do |stanza|
        # upstream bug: stanza.identities and stanza.features always
        # returns an empty array
        features = stanza.xpath('.//ns:feature/@var', :ns => ns).map(&:value)
        expect( features ).to include('my-own-feature')
      end
      @client.handle_data msg
    end
  end
end
