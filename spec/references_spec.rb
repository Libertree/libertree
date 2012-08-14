require 'spec_helper'
require_relative '../lib/libertree/references'

describe Libertree::References do
  before :each do
    @server = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
    @member = Libertree::Model::Member.create(
      FactoryGirl.attributes_for(:member, :server_id => @server.id)
    )
    @server_remote = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
    @member_remote = Libertree::Model::Member.create(
      FactoryGirl.attributes_for(:member, :server_id => @server_remote.id)
    )
  end

  describe 'extract' do
    it 'extracts relative links to local posts' do
      post = Libertree::Model::Post.create(
        FactoryGirl.attributes_for(:post, member_id: @member.id)
      )
      text = "This is a [relative link](/posts/show/#{post.id}). This too: /posts/show/#{post.id}"

      refs = Libertree::References::extract(text, "never-mind.org")
      refs.keys.should include("(/posts/show/#{post.id}")
      refs.keys.should include(" /posts/show/#{post.id}")
    end

    it 'extracts absolute links to local posts' do
      post = Libertree::Model::Post.create(
        FactoryGirl.attributes_for(:post, member_id: @member.id)
      )
      text = "This is an [absolute link](http://never-mind.org/posts/show/#{post.id})."

      refs = Libertree::References::extract(text, "never-mind.org")
      refs.keys.should include("http://never-mind.org/posts/show/#{post.id}")
    end

    it 'does not extract links to remote posts' do
      post = Libertree::Model::Post.create(
        FactoryGirl.attributes_for(:post, member_id: @member.id)
      )
      text = "This is an [absolute link](http://some-tree.org/posts/show/#{post.id})."

      refs = Libertree::References::extract(text, "never-mind.org")
      refs.should be_empty
    end
  end

  describe 'replace' do
    it 'rewrites links to remote resources to point to local copies' do
      post_remote = Libertree::Model::Post.create(
        FactoryGirl.attributes_for(:post, member_id: @member_remote.id, remote_id: nil)
      )
      post_local = Libertree::Model::Post.create(
        FactoryGirl.attributes_for(:post,
          text: post_remote.text,
          member_id: @member_remote.id,
          remote_id: post_remote.id
        )
      )

      original_text =<<EOF
Link 1: /posts/show/#{post_remote.id}
Link 1b: /posts/show/#{post_remote.id}
Link 2: http://never-mind.org/posts/show/#{post_remote.id}
Link 3: [a post](/posts/show/#{post_remote.id})
Link 4: [a post](http://never-mind.org/posts/show/#{post_remote.id})
Link 5: [unchanged](http://remote.org/posts/show/123)
EOF
      expected_text =<<EOF
Link 1: /posts/show/#{post_local.id}
Link 1b: /posts/show/#{post_local.id}
Link 2: /posts/show/#{post_local.id}
Link 3: [a post](/posts/show/#{post_local.id})
Link 4: [a post](/posts/show/#{post_local.id})
Link 5: [unchanged](http://remote.org/posts/show/123)
EOF

      # this happens on the remote server
      refs = Libertree::References::extract(original_text, "never-mind.org")

      # this happens on the receiving server
      processed_text = Libertree::References::replace(original_text, refs, @server_remote.id)
      processed_text.should == expected_text
    end
  end
end
