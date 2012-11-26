require 'spec_helper'

describe Libertree::Model::Comment do
  before do
    @account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
    @member = @account.member
    @post = Libertree::Model::Post.create(
      FactoryGirl.attributes_for( :post, member_id: @member.id, text: 'Example post.' )
    )
  end

  def new_comment(text)
    @comment = Libertree::Model::Comment.create(
      FactoryGirl.attributes_for( :comment, member_id: @member.id, post_id: @post.id, text: text )
    )
  end

  describe '#glimpse' do
    context 'when the text is short' do
      before :each do
        new_comment 'Short text.'
      end
      it 'is all the text' do
        @comment.glimpse.should == 'Short text.'
      end
    end

    context 'when the text is long' do
      before :each do
        new_comment 'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec a diam lectus. Sed sit amet ipsum mauris. Maecenas congue ligula ac quam viverra nec consectetur ante hendrerit.'
      end
      it 'is only some of the beginning of the text when the text is long' do
        @comment.glimpse.should == 'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Don...'
      end
    end

    context 'when the text has a quotation' do
      before :each do
        new_comment "> Here is a quote.\n\nHere is a reply."
      end
      it 'the quotation is not included' do
        @comment.glimpse.should == 'Here is a reply.'
      end
    end
  end
end
