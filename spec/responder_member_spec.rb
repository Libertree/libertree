require 'spec_helper'
require 'net/http'

describe Libertree::Server::Responder::Member do
  let(:subject_class) { Class.new }
  let(:subject) { subject_class.new }

  before :each do
    subject_class.class_eval {
      include Libertree::Server::Responder::Helper
      include Libertree::Server::Responder::Member
    }
  end

  describe 'rsp_member' do
    include_context 'requester in a forest'

    context 'and the member is known' do
      before :each do
        @member = Libertree::Model::Member.create(
          FactoryGirl.attributes_for(:member, :server_id => @requester.id)
        )

        Libertree::Server.stub(:conf) { Hash.new }
        Net::HTTP.any_instance.stub(:get)
        Net::HTTPResponse.any_instance.stub(:body)
        Socket.stub(:getaddrinfo) { [ [nil,nil,nil,@requester.ip] ] }
        File.stub(:open)
        subject.instance_variable_set(:@remote_tree, @requester)
      end

      it 'raises MissingParameter with a missing username' do
        h = {
          'avatar_url' => 'http://libertree.net/images/avatars/1.png',
          'profile' => {
            'name_display' => '',
            'description'  => '',
          }
        }
        expect { subject.rsp_member(h) }.
          to raise_error( Libertree::Server::MissingParameter )
      end

      it 'raises MissingParameter with a blank username' do
        h = {
          'username' => '',
          'avatar_url' => 'http://libertree.net/images/avatars/1.png',
          'profile' => {
            'name_display' => '',
            'description'  => '',
          }
        }
        expect { subject.rsp_member(h) }.
          to raise_error( Libertree::Server::MissingParameter )
      end

      it 'raises an error with a blank profile display name' do
        h = {
          'username' => 'someuser',
          'avatar_url' => 'http://libertree.net/images/avatars/1.png',
          'profile' => {
            'name_display' => '',
            'description'  => '',
          }
        }
        expect { subject.rsp_member(h) }.
          to raise_error( Libertree::Server::InternalError )
      end

      it 'raises no errors with valid data' do
        h = {
          'username' => 'someuser',
          'avatar_url' => 'http://libertree.net/images/avatars/1.png',
          'profile' => {
            'name_display' => 'Some User',
            'description'  => '',
          }
        }
        expect { subject.rsp_member(h) }.
          not_to raise_error
      end
    end
  end
end
