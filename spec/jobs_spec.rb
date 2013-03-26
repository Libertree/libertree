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
      @client.stub(:request)
      Libertree::Client.stub(:new) {|conf| @client }
    end

    describe 'CHAT#perform' do
      it 'calls req_chat with a valid chat message' do
        @client.stub(:req_chat)
        msg = LM::ChatMessage.create( from_member_id: @member.id,
                                      to_member_id: @other_member.id,
                                      text: "hello" )
        params = {
          'chat_message_id' => msg.id,
          'server_id'       => msg.recipient.tree.id,
        }

        @client.should_receive(:req_chat)
        @client.should_receive(:request) {|domain, args|
          domain.should eq msg.recipient.tree.domain
        }
        Jobs::Request::CHAT.perform( params )
      end
    end

    describe 'COMMENT#perform' do
      pending
    end

    describe 'COMMENT_DELETE#perform' do
      pending
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
