require 'spec_helper'

describe Libertree::Server::Responder::Post do
  subject {
    Class.new.new
  }

  before :each do
    subject.class.class_eval {
      include Libertree::Server::Responder::Helper
      include Libertree::Server::Responder::Post
    }
  end

  describe 'rsp_post' do
    include_context 'requester in a forest'

    context 'and the member is known' do
      before :each do
        @member = Libertree::Model::Member.create(
          FactoryGirl.attributes_for(:member, :server_id => @requester.id)
        )
        subject.instance_variable_set(:@remote_tree, @requester)
      end

      it 'raises MissingParameterError with a missing id' do
        h = {
          'username'   => @member.username,
          'visibility' => 'forest',
          'text'       => 'A test post.',
        }
        expect { subject.rsp_post(h) }.
          to raise_error( Libertree::Server::MissingParameterError )
      end

      it 'raises MissingParameterError with a blank id' do
        h = {
          'username'   => @member.username,
          'id'         => '',
          'visibility' => 'forest',
          'text'       => 'A test post.',
        }
        expect { subject.rsp_post(h) }.
          to raise_error( Libertree::Server::MissingParameterError )
      end

      it "raises NotFoundError with a member username that isn't found" do
        h = {
          'username'   => 'nosuchusername',
          'id'         => 4,
          'visibility' => 'forest',
          'text'       => 'A test post.',
        }
        expect { subject.rsp_post(h) }.
          to raise_error( Libertree::Server::NotFoundError )
      end

      context 'with valid post data, and a member that does not belong to the requester' do
        before :each do
          other_server = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
          @member = Libertree::Model::Member.create(
            FactoryGirl.attributes_for(:member, :server_id => other_server.id)
          )
        end

        it 'raises NotFoundError' do
          h = {
            'username'   => @member.username,
            'id'         => 5,
            'visibility' => 'forest',
            'text'       => 'A test post.',
          }
          expect { subject.rsp_post(h) }.
            to raise_error( Libertree::Server::NotFoundError )
        end
      end

      it 'raises no errors with valid data' do
        h = {
          'username'   => @member.username,
          'id'         => 6,
          'visibility' => 'forest',
          'text'       => 'A test post.',
        }
        expect { subject.rsp_post(h) }.
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
          'username'   => @member.username,
          'id'         => 6,
          'visibility' => 'forest',
          'text'       => 'A test post.',
          'references' => refs,
        }
        expect { subject.rsp_post(h) }.
          not_to raise_error
      end

      context 'when a remote post exists already' do
        before :each do
          @post = Libertree::Model::Post.create(
            FactoryGirl.attributes_for(:post, member_id: @member.id)
          )
          @initial_text = @post.text
        end

        it 'updates the post text and stores a post revision' do
          original_text = @post.text
          original_text.should_not == 'edited text'
          Libertree::Model::PostRevision.where( post_id: @post.id ).count.should == 0

          h = {
            'username'   => @member.username,
            'id'         => @post.remote_id,
            'visibility' => 'forest',
            'text'       => 'edited text',
          }
          expect { subject.rsp_post(h) }.
            not_to raise_error

          Libertree::Model::Post[@post.id].text.should == 'edited text'
          revisions = Libertree::Model::PostRevision.where( post_id: @post.id ).all
          revisions.count.should == 1
          revisions[0].text.should == original_text
        end
      end
    end
  end
end
