require 'spec_helper'

describe Libertree::Model::Comment do
  before :each do
    @s = Object.new.extend(Ramaze::Helper::Comment)

    @server = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
    @server2 = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )

    @author = Libertree::Model::Member.create(
      FactoryGirl.attributes_for(:member, :username => "john", :server_id => @server.id)
    )
    @paul = Libertree::Model::Member.create(
      FactoryGirl.attributes_for(:member, :username => "paul", :server_id => @server.id)
    )
    @george = Libertree::Model::Member.create(
      FactoryGirl.attributes_for(:member, :username => "george", :server_id => @server.id)
    )
    @george2 = Libertree::Model::Member.create(
      FactoryGirl.attributes_for(:member, :username => "george", :server_id => @server2.id)
    )
    @post = Libertree::Model::Post.create(
      FactoryGirl.attributes_for(:post, :member_id => @author.id)
    )
  end

  describe '#text_rendered_and_participants_linked' do
    context 'with more than one comment' do
      it 'should turn @name into a link to last comment by user name' do
        Libertree::Model::Comment.create(
          FactoryGirl.attributes_for(:comment,
                                     :member_id => @george.id,
                                     :post_id => @post.id,
                                     :text => "Very interesting."))
        comment = Libertree::Model::Comment.create(
          FactoryGirl.attributes_for(:comment,
                                     :member_id => @paul.id,
                                     :post_id => @post.id,
                                     :text => "@George@#{@george.server.domain}: indeed. I thought you'd like that."))

        commenters = @s.commenters(@post.comments)
        processed = comment.text_rendered_and_participants_linked(commenters)
        processed.should =~ /data-member-id="#{@george.id}"/
      end

      it 'should replace uppercase matches of lowercase names' do
        lower = Libertree::Model::Member.create(
          FactoryGirl.attributes_for(:member, :username => "somename", :server_id => @server.id)
        )
        Libertree::Model::Comment.create(
          FactoryGirl.attributes_for(:comment,
                                     :member_id => lower.id,
                                     :post_id => @post.id,
                                     :text => "Very interesting."))
        comment = Libertree::Model::Comment.create(
          FactoryGirl.attributes_for(:comment,
                                     :member_id => @paul.id,
                                     :post_id => @post.id,
                                     :text => "@SomeName@#{@server.domain}: indeed. I thought you'd like that."))

        commenters = @s.commenters(@post.comments)
        processed = comment.text_rendered_and_participants_linked(commenters)
        processed.should =~ /data-member-id="#{lower.id}"/
      end

      it 'should match only the most recent name in ambiguous situations' do
        Libertree::Model::Comment.create(
          FactoryGirl.attributes_for(:comment,
                                     :member_id => @george.id,
                                     :post_id => @post.id,
                                     :text => "Very interesting."))
        Libertree::Model::Comment.create(
          FactoryGirl.attributes_for(:comment,
                                     :member_id => @george2.id,
                                     :post_id => @post.id,
                                     :text => "Very interesting, indeed."))
        comment = Libertree::Model::Comment.create(
          FactoryGirl.attributes_for(:comment,
                                     :member_id => @paul.id,
                                     :post_id => @post.id,
                                     :text => "@george@#{@george2.server.domain}: I mean you, George 2."))

        commenters = @s.commenters(@post.comments)
        processed = comment.text_rendered_and_participants_linked(commenters)
        processed.should =~ /data-member-id="#{@george2.id}"/
        processed.should_not =~ /data-member-id="#{@george.id}"/
      end

      it 'should handle more than one mention' do
        Libertree::Model::Comment.create(
          FactoryGirl.attributes_for(:comment,
                                     :member_id => @george.id,
                                     :post_id => @post.id,
                                     :text => "Very interesting."))
        Libertree::Model::Comment.create(
          FactoryGirl.attributes_for(:comment,
                                     :member_id => @author.id,
                                     :post_id => @post.id,
                                     :text => "Very interesting, indeed."))
        comment = Libertree::Model::Comment.create(
          FactoryGirl.attributes_for(:comment,
                                     :member_id => @paul.id,
                                     :post_id => @post.id,
                                     :text => "@george@#{@george.server.domain}: I agree. @john@#{@author.server.domain}: I also agree."))

        commenters = @s.commenters(@post.comments)
        processed = comment.text_rendered_and_participants_linked(commenters)
        processed.should =~ /data-member-id="#{@george.id}"/
        processed.should =~ /data-member-id="#{@author.id}"/
      end
   end

    context 'with fewer than two comments' do
      it 'should not do anything on the first comment' do
        comment = Libertree::Model::Comment.create(
          FactoryGirl.attributes_for(:comment,
                                     :member_id => @paul.id,
                                     :post_id => @post.id,
                                     :text => "@George@#{@george.server.domain}: hope you find this interesting."))

        commenters = @s.commenters(@post.comments)
        processed = comment.text_rendered_and_participants_linked(commenters)
        processed.should == comment.text_rendered(nil)
      end
    end

  end
end
