require 'spec_helper'

describe Libertree::Server::Responder::Comment do
  subject {
    Class.new.new
  }

  before :each do
    subject.class.class_eval {
      include Libertree::Server::Responder::Helper
      include Libertree::Server::Responder::Comment
    }
  end

  describe 'rsp_comment' do
    include_context 'requester in a forest'

    context 'and the responder has record of both the member and the post' do
      before :each do
        @member = Libertree::Model::Member.create(
          FactoryGirl.attributes_for(:member, :server_id => @requester.id)
        )
        @post = Libertree::Model::Post.create(
          FactoryGirl.attributes_for(:post, member_id: @member.id)
        )
        subject.instance_variable_set(:@remote_tree, @requester)
      end

      it 'raises MissingParameterError when a parameter is missing or blank' do
        h = {
          'id'       => 999,
          'username' => @member.username,
          'origin'   => @post.member.server.domain,
          'post_id'  => @post.remote_id,
          'text'     => 'A test comment.',
        }

        keys = h.keys
        keys.each do |key|
          h_ = h.reject { |k,v| k == key }
          expect { subject.rsp_comment(h_) }.
            to raise_error( Libertree::Server::MissingParameterError )

          h_ = h.dup
          h_[key] = ''
          expect { subject.rsp_comment(h_) }.
            to raise_error( Libertree::Server::MissingParameterError )
        end
      end

      it "raises NotFoundError with a member username that isn't found" do
        h = {
          'id'       => 999,
          'username' => 'nosuchusername',
          'origin'   => @post.member.server.domain,
          'post_id'  => @post.remote_id,
          'text'     => 'A test comment.',
        }
        expect { subject.rsp_comment(h) }.
          to raise_error( Libertree::Server::NotFoundError )
      end

      it "raises NotFoundError with a post id that isn't found" do
        h = {
          'id'       => 999,
          'username' => @member.username,
          'origin'   => @post.member.server.domain,
          'post_id'  => 99999999,
          'text'     => 'A test comment.',
        }
        expect { subject.rsp_comment(h) }.
          to raise_error( Libertree::Server::NotFoundError )
      end

      context 'with valid comment data, and a member that does not belong to the requester' do
        before :each do
          other_server = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
          @member = Libertree::Model::Member.create(
            FactoryGirl.attributes_for(:member, :server_id => other_server.id)
          )
        end

        it 'raises NotFoundError' do
          h = {
            'id'       => 999,
            'username' => @member.username,
            'origin'   => @post.member.server.domain,
            'post_id'  => @post.remote_id,
            'text'     => 'A test comment.',
          }
          expect { subject.rsp_comment(h) }.
            to raise_error( Libertree::Server::NotFoundError )
        end
      end

      context 'with valid comment data, and a post that does not belong to the requester' do
        before :each do
          other_server = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
          member = Libertree::Model::Member.create(
            FactoryGirl.attributes_for(:member, :server_id => other_server.id)
          )
          @post = Libertree::Model::Post.create(
            FactoryGirl.attributes_for(:post, member_id: member.id)
          )
        end

        it 'raises no errors' do
          h = {
            'id'       => 999,
            'username' => @member.username,
            'origin'   => @post.member.server.domain,
            'post_id'  => @post.remote_id,
            'text'     => 'A test comment.',
          }
          expect { subject.rsp_comment(h) }.
            not_to raise_error
        end
      end

      it 'raises no errors otherwise' do
        h = {
          'id'       => 999,
          'username' => @member.username,
          'origin'   => @post.member.server.domain,
          'post_id'  => @post.remote_id,
          'text'     => 'A test comment.',
        }
        expect { subject.rsp_comment(h) }.
          not_to raise_error
      end

      it 'raises no errors with references' do
        refs = [ { 'reference' =>
                   { "match" => "(/posts/show/366",
                     "post" => {
                       "url" => "/posts/show/366",
                       "id" => 366,
                       "origin" => "some.remote.tree" }}
                 },
                 { 'reference' =>
                   { "match" => " /posts/show/366/128#comment-128",
                     "post" => {
                       "url" => "/posts/show/366",
                       "id" => 365,
                       "origin" => "some.remote.tree" },
                     "comment" => {
                       "url" => "/128#comment-128",
                       "id" => 127,
                       "origin" => "some.remote.tree" }}
                 },
                 { 'reference' =>
                   { "match" => "http://never-mind.org/posts/show/366/128",
                     "post" => {
                       "url" => "/posts/show/366",
                       "id" => 365,
                       "origin" => "some.remote.tree" },
                     "comment" => {
                       "url" => "/128",
                       "id" => 127,
                       "origin" => "some.remote.tree" }}
                 }]
        h = {
          'id'       => 999,
          'username' => @member.username,
          'origin'   => @post.member.server.domain,
          'post_id'  => @post.remote_id,
          'text'     => 'A test comment.',
          'references' => refs,
        }
        expect { subject.rsp_comment(h) }.
          not_to raise_error
      end

      it 'raises no errors with a single reference' do
        refs = { 'reference' => {
            "match" => "(/posts/show/366",
            "post" => {
              "url" => "/posts/show/366",
              "id" => "366",
              "origin" => "some.remote.tree" }
          }
        }

        h = {
          'id'       => 999,
          'username' => @member.username,
          'origin'   => @post.member.server.domain,
          'post_id'  => @post.remote_id,
          'text'     => 'A test comment.',
          'references' => refs,
        }
        expect { subject.rsp_comment(h) }.
          not_to raise_error
      end

    end
  end
end
