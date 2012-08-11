require 'spec_helper'
require_relative '../lib/jobs'

describe Jobs::Request do
  describe 'extract_references' do
    it 'extracts relative links to local posts' do
      @requester = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
      @member = Libertree::Model::Member.create(
        FactoryGirl.attributes_for(:member, :server_id => @requester.id)
      )
      @post = Libertree::Model::Post.create(
        FactoryGirl.attributes_for(:post, member_id: @member.id)
      )
      Jobs::Request.instance_variable_set(:@client_conf, {server_name: "never-mind.org"})
      text = "This is a [relative link](/posts/show/#{@post.id}). This too: /posts/show/#{@post.id}"

      refs = Jobs::Request::extract_references(text)
      refs.keys.should include("(/posts/show/#{@post.id}")
      refs.keys.should include(" /posts/show/#{@post.id}")
    end

    it 'extracts absolute links to local posts' do
      @requester = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
      @member = Libertree::Model::Member.create(
        FactoryGirl.attributes_for(:member, :server_id => @requester.id)
      )
      @post = Libertree::Model::Post.create(
        FactoryGirl.attributes_for(:post, member_id: @member.id)
      )
      Jobs::Request.instance_variable_set(:@client_conf, {server_name: "never-mind.org"})
      text = "This is an [absolute link](http://never-mind.org/posts/show/#{@post.id})."

      refs = Jobs::Request::extract_references(text)
      refs.keys.should include("http://never-mind.org/posts/show/#{@post.id}")
    end

    it 'does not extract links to remote posts' do
      @requester = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
      @member = Libertree::Model::Member.create(
        FactoryGirl.attributes_for(:member, :server_id => @requester.id)
      )
      @post = Libertree::Model::Post.create(
        FactoryGirl.attributes_for(:post, member_id: @member.id)
      )
      Jobs::Request.instance_variable_set(:@client_conf, {server_name: "never-mind.org"})
      text = "This is an [absolute link](http://some-tree.org/posts/show/#{@post.id})."

      refs = Jobs::Request::extract_references(text)
      refs.keys.should == []
    end
  end
end
