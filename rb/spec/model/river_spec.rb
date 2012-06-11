require 'spec_helper'

describe Libertree::Model::River do
  describe '#query_components' do
    before do
      @account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
    end

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
  end
end
