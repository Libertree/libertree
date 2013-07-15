require 'spec_helper'
require_relative '../lib/libertree/references'

describe Libertree::References do
  before :each do
    @server = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
    @member = Libertree::Model::Member.create(
      FactoryGirl.attributes_for(:member, :server_id => @server.id)
    )
    @server_remote = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
    @server_remote.domain = "some.remote.tree"
    @member_remote = Libertree::Model::Member.create(
      FactoryGirl.attributes_for(:member, :server_id => @server_remote.id)
    )
    @domain = @server.domain
  end

  describe 'extract' do
    it 'extracts relative links to local posts' do
      post = Libertree::Model::Post.create(
        FactoryGirl.attributes_for(:post, member_id: @member.id)
      )
      text = "This is a [relative link](/posts/show/#{post.id}). This too: /posts/show/#{post.id}"

      refs = Libertree::References::extract(text, "http://never-mind.org")
      refs.keys.should include("(/posts/show/#{post.id}")
      refs.keys.should include(" /posts/show/#{post.id}")
    end

    it 'extracts absolute links to local posts' do
      post = Libertree::Model::Post.create(
        FactoryGirl.attributes_for(:post, member_id: @member.id)
      )
      text = "This is an [absolute link](http://never-mind.org/posts/show/#{post.id})."

      refs = Libertree::References::extract(text, "http://never-mind.org")
      refs.keys.should include("http://never-mind.org/posts/show/#{post.id}")
    end

    it 'does not extract links to remote posts' do
      post = Libertree::Model::Post.create(
        FactoryGirl.attributes_for(:post, member_id: @member.id)
      )
      text = "This is an [absolute link](http://some-tree.org/posts/show/#{post.id})."

      refs = Libertree::References::extract(text, "http://never-mind.org")
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
      refs = Libertree::References::extract(original_text, "http://never-mind.org")

      # this happens on the receiving server
      processed_text = Libertree::References::replace(original_text, refs, @server_remote.id, @domain)
      processed_text.should == expected_text
    end

    it 'rewrites links that point to posts that originate here' do
      post_original = Libertree::Model::Post.create(
        FactoryGirl.attributes_for(:post, member_id: @member.id, remote_id: nil)
      )
      post_remote_copy = Libertree::Model::Post.create(
        FactoryGirl.attributes_for(:post,
          text: post_original.text,
          member_id: @member.id,
          remote_id: post_original.id
        )
      )

      original_text =<<EOF
Link 1: /posts/show/#{post_remote_copy.id}
Link 1b: /posts/show/#{post_remote_copy.id}
Link 2: http://never-mind.org/posts/show/#{post_remote_copy.id}
Link 3: [a post](/posts/show/#{post_remote_copy.id})
Link 4: [a post](http://never-mind.org/posts/show/#{post_remote_copy.id})
Link 5: [unchanged](http://remote.org/posts/show/123)
EOF
      expected_text =<<EOF
Link 1: /posts/show/#{post_original.id}
Link 1b: /posts/show/#{post_original.id}
Link 2: /posts/show/#{post_original.id}
Link 3: [a post](/posts/show/#{post_original.id})
Link 4: [a post](/posts/show/#{post_original.id})
Link 5: [unchanged](http://remote.org/posts/show/123)
EOF

      # this happens on the remote server
      refs = Libertree::References::extract(original_text, "http://never-mind.org")

      # this happens on the receiving server
      processed_text = Libertree::References::replace(original_text, refs, @server_remote.id, @domain)
      processed_text.should == expected_text
    end

    it 'rewrites links that point to comments that originate here' do
      post_original = Libertree::Model::Post.create(
        FactoryGirl.attributes_for(:post, member_id: @member.id, remote_id: nil)
      )
      comment_original = Libertree::Model::Comment.create(
        FactoryGirl.attributes_for(:comment,
          member_id: @member.id,
          remote_id: nil,
          post_id: post_original.id
        )
      )
      post_remote_copy = Libertree::Model::Post.create(
        FactoryGirl.attributes_for(:post,
          text: post_original.text,
          member_id: @member.id,
          remote_id: post_original.id
        )
      )
      comment_remote_copy = Libertree::Model::Comment.create(
        FactoryGirl.attributes_for(:comment,
          member_id: @member.id,
          remote_id: nil,
          post_id: post_remote_copy.id,
          remote_id: comment_original.id,
        )
      )

      original_text =<<EOF
Link P1: /posts/show/#{post_remote_copy.id}
Link P1b: /posts/show/#{post_remote_copy.id}
Link P2: http://never-mind.org/posts/show/#{post_remote_copy.id}
Link P3: [a post](/posts/show/#{post_remote_copy.id})
Link P4: [a post](http://never-mind.org/posts/show/#{post_remote_copy.id})
Link P5: [unchanged](http://remote.org/posts/show/123)

Link C1: /posts/show/#{post_remote_copy.id}/#{comment_remote_copy.id}
Link C2: /posts/show/#{post_remote_copy.id}/#{comment_remote_copy.id}
Link C3: /posts/show/#{post_remote_copy.id}/#{comment_remote_copy.id}#comment-#{comment_remote_copy.id}
Link C4: http://never-mind.org/posts/show/#{post_remote_copy.id}/#{comment_remote_copy.id}
Link C5: [a comment](/posts/show/#{post_remote_copy.id}/#{comment_remote_copy.id})
Link C6: [a comment](http://never-mind.org/posts/show/#{post_remote_copy.id}/#{comment_remote_copy.id})
Link C7: [unchanged](http://remote.org/posts/show/123)
Link C8: [unchanged](http://remote.org/posts/show/123/#{comment_remote_copy.id})
EOF
      expected_text =<<EOF
Link P1: /posts/show/#{post_original.id}
Link P1b: /posts/show/#{post_original.id}
Link P2: /posts/show/#{post_original.id}
Link P3: [a post](/posts/show/#{post_original.id})
Link P4: [a post](/posts/show/#{post_original.id})
Link P5: [unchanged](http://remote.org/posts/show/123)

Link C1: /posts/show/#{post_original.id}/#{comment_original.id}
Link C2: /posts/show/#{post_original.id}/#{comment_original.id}
Link C3: /posts/show/#{post_original.id}/#{comment_original.id}#comment-#{comment_original.id}
Link C4: /posts/show/#{post_original.id}/#{comment_original.id}
Link C5: [a comment](/posts/show/#{post_original.id}/#{comment_original.id})
Link C6: [a comment](/posts/show/#{post_original.id}/#{comment_original.id})
Link C7: [unchanged](http://remote.org/posts/show/123)
Link C8: [unchanged](http://remote.org/posts/show/123/#{comment_remote_copy.id})
EOF

      # this happens on the remote server
      refs = Libertree::References::extract(original_text, "http://never-mind.org")

      # this happens on the receiving server
      processed_text = Libertree::References::replace(original_text, refs, @server_remote.id, @domain)
      processed_text.should == expected_text
    end

    it 'does not replace text that happens to be the same as a comment id' do
      post_original = Libertree::Model::Post.create(
        FactoryGirl.attributes_for(:post, member_id: @member.id, remote_id: nil)
      )
      comment_original = Libertree::Model::Comment.create(
        FactoryGirl.attributes_for(:comment,
          member_id: @member.id,
          remote_id: nil,
          post_id: post_original.id
        )
      )
      post_remote_copy = Libertree::Model::Post.create(
        FactoryGirl.attributes_for(:post,
          text: post_original.text,
          member_id: @member.id,
          remote_id: post_original.id
        )
      )
      comment_remote_copy = Libertree::Model::Comment.create(
        FactoryGirl.attributes_for(:comment,
          member_id: @member.id,
          remote_id: nil,
          post_id: post_remote_copy.id,
          remote_id: comment_original.id,
        )
      )

      original_text =<<EOF
Link C1: /posts/show/#{post_remote_copy.id}/#{comment_remote_copy.id}
unchanged: This looks like a part of a comment URL /#{comment_remote_copy.id}
EOF
      expected_text =<<EOF
Link C1: /posts/show/#{post_original.id}/#{comment_original.id}
unchanged: This looks like a part of a comment URL /#{comment_remote_copy.id}
EOF

      # this happens on the remote server
      refs = Libertree::References::extract(original_text, "http://never-mind.org")

      # this happens on the receiving server
      processed_text = Libertree::References::replace(original_text, refs, @server_remote.id, @domain)
      processed_text.should == expected_text
    end
  end
end
