require 'spec_helper'

describe Libertree::Model::NodeSubscription do
  describe '.for' do
    before :all do
      @node_1 = Libertree::Model::Node.create( FactoryGirl.attributes_for(:node) )
      @node_2 = Libertree::Model::Node.create( FactoryGirl.attributes_for(:node) )
      @node_3 = Libertree::Model::Node.create( FactoryGirl.attributes_for(:node) )

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

    it 'returns subscriptions for all users on a given host if a hostname is provided' do
      subs = Libertree::Model::NodeSubscription.for('example.localdomain').all
      expect( subs ).to match_array(@example_subs)
      expect( subs ).not_to match_array(@other_subs)
    end

    it 'returns all of a user\'s subscriptions when a jid is provided' do
      subs = Libertree::Model::NodeSubscription.for('a@example.localdomain')
      expect( subs.count ).to be(2)
      expect( subs.map(&:jid).uniq ).to match_array(['a@example.localdomain'])
      expect( subs.map(&:node_id) ).to match_array([@node_1, @node_2].map(&:id))
    end
  end
end
