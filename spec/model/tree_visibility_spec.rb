# encoding: utf-8
require 'spec_helper'

describe Libertree::Model::Job do
  before do
    remote = Libertree::Model::Server.create(
      FactoryGirl.attributes_for(:server, name_given: 'remote' )
    )
    forest = Libertree::Model::Forest.create(FactoryGirl.attributes_for(:forest, local_is_member: true))
    forest.add(remote)
    @account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
    @member = @account.member
  end

  describe 'tree visibility' do
    it 'does not trigger a distribution job' do
      Libertree::Model::Job.should_not_receive(:create_for_forests)

      @post =
        Libertree::Model::Post.create(member_id: @member.id, text: 'text', visibility: 'tree', remote_id: nil)
      @comment =
        Libertree::Model::Comment.create(member_id: @member.id, post_id: @post.id, text: 'text', remote_id: nil)
      @post_like =
        Libertree::Model::PostLike.create(member_id: @member.id, post_id: @post.id)
      @comment_like =
        Libertree::Model::CommentLike.create(member_id: @member.id, comment_id: @comment.id)

      @comment_like.delete_cascade
      @comment.delete_cascade
      @post_like.delete_cascade
      @post.delete_cascade
    end
  end

end
