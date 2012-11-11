# encoding: utf-8
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

  describe '#with_tags' do
    context 'expecting posts tagged with #test' do
      opts = { :tag => 'test' }
      it 'returns posts that are tagged with #test' do
        post = new_post 'This is tagged with #test.'
        Libertree::Model::Post.with_tag( opts ).should include(post)
      end

      it 'does not return posts that are tagged with #testing' do
        post = new_post 'This is tagged with #testing.'
        Libertree::Model::Post.with_tag( opts ).should_not include(post)
      end

      it 'does not return posts that contain "invalid#test"' do
        post = new_post 'This is not tagged invalid#test.'
        Libertree::Model::Post.with_tag( opts ).should_not include(post)
      end

      it 'matches posts tagged at the beginning of the post' do
        post = new_post '#test or not to test.'
        Libertree::Model::Post.with_tag( opts ).should include(post)
      end

      it 'matches posts tagged at the beginning of a line' do
        post = new_post "nothing special\n#test or not to test."
        Libertree::Model::Post.with_tag( opts ).should include(post)
      end
    end

    context 'with unicode tags' do
      opts = { :tag => '你好' }
      it 'returns posts that are tagged with #你好' do
        post = new_post 'This is tagged with #你好.'
        Libertree::Model::Post.with_tag( opts ).should include(post)
      end

      it 'does not return posts that are tagged with #你好吗' do
        post = new_post 'This is tagged with #你好吗.'
        Libertree::Model::Post.with_tag( opts ).should_not include(post)
      end

      it 'does not return posts that contain "好#你好"' do
        post = new_post 'This is not tagged 好#你好.'
        Libertree::Model::Post.with_tag( opts ).should_not include(post)
      end
    end

    context 'with accented character tags' do
      opts = { :tag => 'José' }
      it 'returns posts that are tagged with #José' do
        post = new_post 'This is tagged with #José.'
        Libertree::Model::Post.with_tag( opts ).should include(post)
      end

      it 'does not return posts that are tagged with #Josém' do
        post = new_post 'This is tagged with #Josém.'
        Libertree::Model::Post.with_tag( opts ).should_not include(post)
      end

      it 'does not return posts that contain "no#José"' do
        post = new_post 'This is not tagged no#José.'
        Libertree::Model::Post.with_tag( opts ).should_not include(post)
      end
    end
  end
end
