require 'blather'
require 'spec_helper'
require 'libertree/client'

describe Libertree::Server::PubSub do
  before :each do
    @client = LSR.connection
    @client.stub :write
    @jid = 'pubsub.liber.tree'
    Libertree::Server::PubSub.init(@client, @jid)
  end

  before :all do
    @node_1 = Libertree::Model::Node.create( FactoryGirl.attributes_for(:node, address: '/posts') )
    @node_2 = Libertree::Model::Node.create( FactoryGirl.attributes_for(:node, address: '/groups') )
  end

  it 'advertises pubsub service' do
    msg = Blather::Stanza::Iq::DiscoInfo.new
    msg.to = @jid
    ns = msg.class.registered_ns

    expect( @client ).to receive(:write) do |stanza|
      # TODO: upstream bug: stanza.identities and stanza.features
      # always returns an empty array, unless inherited
      stanza = Blather::Stanza::Iq::DiscoInfo.new.inherit stanza
      identity = Blather::Stanza::Iq::DiscoInfo::Identity.
        new({ name: "Libertree PubSub", type: "service", category: "pubsub" })
      expect( stanza.identities ).to include(identity)

      expect( stanza.features.map(&:var) ).to include('http://jabber.org/protocol/pubsub',
                                                      'http://jabber.org/protocol/disco#items')
    end
    @client.handle_data msg
  end

  it 'reports collection or leaf info for pubsub nodes' do
    account = Libertree::Model::Account.create(FactoryGirl.attributes_for(:account))
    spring = Libertree::Model::Pool.create(
      FactoryGirl.attributes_for(:pool, member_id: account.member.id, sprung: true, name: 'whocares')
    )

    collection_nodes =
      [ "/users",
        "/groups",
        "/users/#{account.username}/springs",
      ]

    leaf_nodes =
      [ "/posts",
        "/users/#{account.username}/springs/#{spring.id}",
        "/users/#{account.username}/posts",
      ]

    collection_nodes.each do |node|
      msg = Blather::Stanza::Iq::DiscoInfo.new
      msg.to = @jid
      msg.node = node

      expect( @client ).to receive(:write) do |stanza|
        # TODO: upstream bug: stanza.identities and stanza.features
        # always returns an empty array, unless inherited
        stanza = Blather::Stanza::Iq::DiscoInfo.new.inherit stanza
        identity = Blather::Stanza::Iq::DiscoInfo::Identity.
          new({ type: "collection", category: "pubsub" })
        expect( stanza.identities ).to include(identity)
      end
      @client.handle_data msg
    end

    leaf_nodes.each do |node|
      msg = Blather::Stanza::Iq::DiscoInfo.new
      msg.to = @jid
      msg.node = node

      expect( @client ).to receive(:write) do |stanza|
        # TODO: upstream bug: stanza.identities and stanza.features
        # always returns an empty array, unless inherited
        stanza = Blather::Stanza::Iq::DiscoInfo.new.inherit stanza
        identity = Blather::Stanza::Iq::DiscoInfo::Identity.
          new({ type: "leaf", category: "pubsub" })
        expect( stanza.identities ).to include(identity)
      end
      @client.handle_data msg
    end
  end

  describe 'retrieve_subscriptions' do
    before :all do
      @example_subs = []
      @other_subs = []

      [ @node_1, @node_2 ].each do |node|
        [ 'a', 'b', 'c' ].each do |jid|
          sub = Libertree::Model::NodeSubscription.
            create(FactoryGirl.attributes_for(:node_subscription,
                                              jid: "#{jid}@example.localdomain",
                                              node_id: node.id))
          @example_subs << sub
        end
      end

      [ @node_1, @node_2 ].each do |node|
        [ 'x', 'y', 'z' ].each do |jid|
          sub = Libertree::Model::NodeSubscription.
            create(FactoryGirl.attributes_for(:node_subscription,
                                              jid: "#{jid}@other.domain",
                                              node_id: node.id))
          @other_subs << sub
        end
      end
    end

    it 'fails with unsupported error if an unknown node is queried' do
      msg = Blather::Stanza::PubSub::Subscriptions.new(:get, @jid)
      # TODO: upstream bug: msg.subscriptions duplicates the subscriptions node
      msg.pubsub.find_first('ns:subscriptions', ns: msg.class.registered_ns).write_attr('node', 'this-node-does-not-exist')
      msg.from = 'some@jid'

      expect( @client ).to receive(:write) do |stanza|
        expect( stanza.to_s ).to match(%r{feature-not-implemented})
      end
      @client.handle_data msg
    end

    it 'returns all subscriptions for a given host if the requester is a host and no node has been passed' do
      msg = Blather::Stanza::PubSub::Subscriptions.new(:get, @jid)
      msg.from = 'example.localdomain'

      expect( @client ).to receive(:write) do |stanza|
        expect( stanza.list.values.flatten.count ).to eq(6) # all subs for people at example.localdomain
      end
      @client.handle_data msg
    end

    it 'returns all subscriptions for a given host and node if the requester is a host and a node has been passed' do
      msg = Blather::Stanza::PubSub::Subscriptions.new(:get, @jid)
      msg.from = 'example.localdomain'
      # TODO: upstream bug: msg.subscriptions duplicates the subscriptions node
      msg.pubsub.find_first('ns:subscriptions', ns: msg.class.registered_ns).write_attr('node', @node_1.address)

      expect( @client ).to receive(:write) do |stanza|
        expect( stanza.list.values.flatten.count ).to eq(3) # all subs to node_1 for people at example.localdomain
      end
      @client.handle_data msg
    end

    it 'returns all subscriptions for a given jid if the requester is a jid and no node has been passed' do
      msg = Blather::Stanza::PubSub::Subscriptions.new(:get, @jid)
      msg.from = 'a@example.localdomain'

      expect( @client ).to receive(:write) do |stanza|
        expect( stanza.list.values.flatten.count ).to eq(2) # all subs by a@example.localdomain
      end
      @client.handle_data msg
    end

    it 'returns all subscriptions for a given jid and node if the requester is a jid and a node has been passed' do
      msg = Blather::Stanza::PubSub::Subscriptions.new(:get, @jid)
      msg.from = 'a@example.localdomain'
      # TODO: upstream bug: msg.subscriptions duplicates the subscriptions node
      msg.pubsub.find_first('ns:subscriptions', ns: msg.class.registered_ns).write_attr('node', @node_1.address)

      expect( @client ).to receive(:write) do |stanza|
        expect( stanza.list.values.flatten.count ).to eq(1) # all subs to node_1 for a@example.localdomain
      end
      @client.handle_data msg
    end
  end

  describe 'retrieve_affiliations' do
    before :all do
      @example_affs = []
      @other_affs = []

      [ @node_1, @node_2 ].each do |node|
        [ 'a', 'b', 'c' ].each do |jid|
          aff = Libertree::Model::NodeAffiliation.
            create(FactoryGirl.attributes_for(:node_affiliation,
                                              jid: "#{jid}@example.localdomain",
                                              node_id: node.id))
          @example_affs << aff
        end
      end

      [ @node_1, @node_2 ].each do |node|
        [ 'x', 'y', 'z' ].each do |jid|
          aff = Libertree::Model::NodeAffiliation.
            create(FactoryGirl.attributes_for(:node_affiliation,
                                              jid: "#{jid}@other.domain",
                                              node_id: node.id))
          @other_affs << aff
        end
      end
    end

    it 'fails with unsupported error if an unknown node is queried' do
      msg = Blather::Stanza::PubSub::Affiliations.new(:get, @jid)
      msg.affiliations.write_attr('node', 'this-node-does-not-exist')
      msg.from = 'some@jid'

      expect( @client ).to receive(:write) do |stanza|
        expect( stanza.to_s ).to match(%r{feature-not-implemented})
      end
      @client.handle_data msg
    end

    it 'returns all affiliations for a given jid if the requester is a jid and no node has been passed' do
      msg = Blather::Stanza::PubSub::Affiliations.new(:get, @jid)
      msg.from = 'a@example.localdomain'

      expect( @client ).to receive(:write) do |stanza|
        expect( stanza.list.values.flatten.count ).to eq(2) # all affiliations by a@example.localdomain
      end
      @client.handle_data msg
    end

    it 'returns all affiliations for a given jid and node if the requester is a jid and a node has been passed' do
      msg = Blather::Stanza::PubSub::Affiliations.new(:get, @jid)
      msg.from = 'a@example.localdomain'
      msg.affiliations.write_attr('node', @node_1.address)

      expect( @client ).to receive(:write) do |stanza|
        expect( stanza.list.values.flatten.count ).to eq(1) # all affiliations on node_1 for a@example.localdomain
      end
      @client.handle_data msg
    end
  end

end
