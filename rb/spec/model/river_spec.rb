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
  end
end
