require 'spec_helper'

describe Libertree::Model::River do
  before do
    @account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
  end

  describe '#query_components' do

    def test_one( query, expected )
      river = Libertree::Model::River.create(
        FactoryGirl.attributes_for( :river, label: query, query: query, account_id: @account.id )
      )
      river.query_components.should == expected
    end

    it "splits the text of the river's query along spaces into an Array of words" do
      test_one  'test', ['test']
      test_one  'test two', ['test', 'two']
      test_one  'a b c', ['a', 'b', 'c']
      test_one  '1 _ !', ['1', '_', '!']
    end

    it 'treats quoted strings as single components' do
      test_one  %{match "as is"}, [ 'match', 'as is' ]
    end

    it 'ignores unmatched quotes' do
      test_one  %{match "as is}, [ 'match', '"as', 'is' ]
      test_one  %{match "as is" "hey there}, [ 'match', 'as is', '"hey', 'there' ]
      test_one  %{match "as is" hey" there}, [ 'match', 'as is', 'hey"', 'there' ]
    end

    it 'ignores quotes inside words' do
      test_one  %{match a"s is}, [ 'match', 'a"s', 'is' ]
      test_one  %{match a"s i"s}, [ 'match', 'a"s', 'i"s' ]
    end

    it 'treats "minused" quoted strings as single components' do
      test_one  %{match -"as is" yo}, [ 'match', '-as is', 'yo' ]
    end
  end

  describe '#try_post' do
    before do
      other_account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
      @member = Libertree::Model::Member.create(
        FactoryGirl.attributes_for( :member, :account_id => other_account.id, username: nil )
      )
    end

    def try_one( query, post_text, should_match = true )
      Libertree::DB.dbh.d  "DELETE FROM river_posts"
      Libertree::DB.dbh.d  "DELETE FROM rivers"
      river = Libertree::Model::River.create(
        FactoryGirl.attributes_for( :river, label: query, query: query, account_id: @account.id )
      )

      if should_match
        expect {
          post = Libertree::Model::Post.create(
            FactoryGirl.attributes_for( :post, member_id: @member.id, text: post_text )
          )
        }.to change { Libertree::DB.dbh.sc "SELECT COUNT(*) FROM river_posts" }.by(1)
      else
        expect {
          post = Libertree::Model::Post.create(
            FactoryGirl.attributes_for( :post, member_id: @member.id, text: post_text )
          )
        }.not_to change { Libertree::DB.dbh.sc "SELECT COUNT(*) FROM river_posts" }
      end
    end

    it 'matches in basic cases' do
      try_one  'test', 'no match', false
      try_one  'test', 'test', true
      try_one  'test', 'and test', true
    end
  end
end
