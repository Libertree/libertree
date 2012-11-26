require 'spec_helper'

describe Libertree::Model::Post do
  before do
    @account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
    @member = @account.member
  end

  def new_post(text)
    @post = Libertree::Model::Post.create(
      FactoryGirl.attributes_for( :post, member_id: @member.id, text: text )
    )
  end

  describe '#glimpse' do
    context 'when the text is short' do
      before :each do
        new_post 'Short text.'
      end
      it 'is all the text' do
        @post.glimpse.should == 'Short text.'
      end
    end

    context 'when the text is long' do
      before :each do
        new_post 'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec a diam lectus. Sed sit amet ipsum mauris. Maecenas congue ligula ac quam viverra nec consectetur ante hendrerit.'
      end
      it 'is only some of the beginning of the text when the text is long' do
        @post.glimpse.should == 'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Don...'
      end
    end
  end
end
