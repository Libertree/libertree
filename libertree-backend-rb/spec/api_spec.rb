require 'blather'
require 'spec_helper'

describe Libertree::Server::Api do
  before :each do
    @client = LSR.connection
    @client.stub :write
  end
  before :all do
    @gateway = "gateway.liber.tree"
    @jid = "tester@test.net"
    @account = Libertree::Model::Account.create({
      username: "username",
      password_encrypted: BCrypt::Password.create("1234"),
      gateway_jid: @jid
    })
  end

  it 'performs Api.post on receiving a message starting with "POST"' do
    msg = Blather::Stanza::Message.new
    msg.to = @gateway
    msg.body = "POST hello world"
    msg.from = @account.gateway_jid

    expect( Libertree::Server::Api ).to receive(:post)
    @client.handle_data msg

    msg.body = "post hello world"
    expect( Libertree::Server::Api ).not_to receive(:post)
    @client.handle_data msg
  end

  it 'performs Api.reply on receiving a message starting with "REPLY 1234"' do
    msg = Blather::Stanza::Message.new
    msg.to = @gateway
    msg.body = "REPLY 2345 this is a reply to post 2345"
    msg.from = @account.gateway_jid

    expect( Libertree::Server::Api ).to receive(:reply)
    @client.handle_data msg

    msg.body = "reply 2345 this is a reply to post 2345"
    expect( Libertree::Server::Api ).not_to receive(:reply)
    @client.handle_data msg
  end

  describe 'post' do
    it 'creates a new post' do
      count = Libertree::Model::Post.count
      Libertree::Server::Api.post(@account, "a message")
      expect( Libertree::Model::Post.count ).to eq(count + 1)
    end
  end

  describe 'reply' do
    it 'creates a new comment if the specified post exists' do
       post = Libertree::Model::Post.create(
         member_id:  @account.member.id,
         visibility: 'tree',
         text:       'test',
         via:        nil
       )

      count = post.comments.count
      Libertree::Server::Api.reply(@account, post.id, "a comment")
      expect( post.comments.count ).to eq(count + 1)
    end
  end
end
