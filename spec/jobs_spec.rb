require 'spec_helper'
require 'libertree/client'
require_relative '../lib/jobs'

describe Jobs do
  LM = Libertree::Model
  before :each do
    @server =
      LM::Server.create( FactoryGirl.attributes_for(:server) )
    @server.domain = "here"
    @other_server =
      LM::Server.create( FactoryGirl.attributes_for(:server) )
    @other_server.domain = "faraway"
    @member =
      LM::Member.create( FactoryGirl.attributes_for(:member, :server_id => @server.id) )
    @other_member =
      LM::Member.create( FactoryGirl.attributes_for(:member, :server_id => @other_server.id) )
  end

  describe Jobs::River do
    describe 'Refresh#perform' do
      it 'calls #refresh_posts on the identified river' do
        pending
      end
    end

    describe 'RefreshAll#perform' do
      it "calls #refresh_posts on all of the account's rivers" do
        pending
      end
    end
  end

  describe Jobs::Request do
    before :each do
      @client = mock("Client")
      @client.stub(:request) {|domain, args| {'code' => 'OK'}}
      Jobs::Request.stub(:client) { @client }
    end

    describe 'CHAT#perform' do
      it 'calls req_chat with a valid chat message' do
        msg = LM::ChatMessage.create( from_member_id: @member.id,
                                      to_member_id: @other_member.id,
                                      text: "hello" )
        params = {
          'chat_message_id' => msg.id,
          'server_id'       => msg.recipient.tree.id,
        }

        @client.stub(:req_chat)
        @client.should_receive(:req_chat)
        Jobs::Request::CHAT.perform( params )
      end
    end

    describe 'COMMENT#perform' do
      it 'calls req_comment with a valid comment' do
        # remote post
        post = LM::Post.create( member_id: @other_member.id,
                                text: "this is a post" )
        comment = LM::Comment.create( member_id: @member.id,
                                      post_id: post.id,
                                      text: "hello" )
        params = {
          'comment_id' => comment.id,
          'server_id'  => post.member.server_id,
        }
        @client.stub(:req_comment)
        @client.should_receive(:req_comment)
        @client.should_receive(:request).with(comment.post.member.tree.domain, anything())
        Jobs::Request::COMMENT.perform( params )
      end
    end

    describe 'COMMENT_DELETE#perform' do
      it 'calls req_comment_delete with a valid comment' do
        # remote post
        post = LM::Post.create( member_id: @other_member.id,
                                text: "this is a post" )
        comment = LM::Comment.create( member_id: @member.id,
                                      post_id: post.id,
                                      text: "hello" )
        params = {
          'comment_id' => comment.id,
          'server_id'  => post.member.server_id,
        }
        @client.stub(:req_comment_delete)
        @client.should_receive(:req_comment_delete)
        @client.should_receive(:request).with(comment.post.member.tree.domain, anything())
        Jobs::Request::COMMENT_DELETE.perform( params )
      end
    end

    describe 'COMMENT_LIKE#perform' do
      pending
    end

    describe 'COMMENT_LIKE_DELETE#perform' do
      pending
    end

    describe 'FOREST#perform' do
      pending
    end

    describe 'MEMBER#perform' do
      pending
    end

    describe 'MEMBER_DELETE#perform' do
      pending
    end

    describe 'MESSAGE#perform' do
      pending
    end

    describe 'POOL#perform' do
      pending
    end

    describe 'POOL_DELETE#perform' do
      pending
    end

    describe 'POOL_POST#perform' do
      pending
    end

    describe 'POOL_POST_DELETE#perform' do
      pending
    end

    describe 'POST#perform' do
      pending
    end

    describe 'POST_DELETE#perform' do
      pending
    end

    describe 'POST_LIKE#perform' do
      pending
    end

    describe 'POST_LIKE_DELETE#perform' do
      pending
    end
  end
end
