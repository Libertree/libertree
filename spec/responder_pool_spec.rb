require 'spec_helper'

describe Libertree::Server::Responder::Pool do
  let(:subject_class) { Class.new }
  let(:subject) { subject_class.new }

  before :each do
    subject_class.class_eval {
      include Libertree::Server::Responder::Helper
      include Libertree::Server::Responder::Pool
    }
  end

  describe 'rsp_pool' do
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
          'name'       => 'Pool Name',
        }
        expect { subject.rsp_pool(h) }.
          to raise_error( Libertree::Server::MissingParameterError )
      end

      it 'raises MissingParameterError with a blank id' do
        h = {
          'username'   => @member.username,
          'id'         => '',
          'name'       => 'Pool Name',
        }
        expect { subject.rsp_pool(h) }.
          to raise_error( Libertree::Server::MissingParameterError )
      end

      it "raises NotFound with a member username that isn't found" do
        h = {
          'username'   => 'nosuchusername',
          'id'         => 4,
          'name'       => 'Pool Name',
        }
        expect { subject.rsp_pool(h) }.
          to raise_error( Libertree::Server::NotFound )
      end

      context 'with valid pool data, and a member that does not belong to the requester' do
        before :each do
          other_server = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
          @member = Libertree::Model::Member.create(
            FactoryGirl.attributes_for(:member, :server_id => other_server.id)
          )
        end

        it 'raises NotFound' do
          h = {
            'username'   => @member.username,
            'id'         => 4,
            'name'       => 'Pool Name',
          }
          expect { subject.rsp_pool(h) }.
            to raise_error( Libertree::Server::NotFound )
        end
      end

      it 'raises no errors with valid data' do
        h = {
          'username'   => @member.username,
          'id'         => 4,
          'name'       => 'Pool Name',
        }
        expect { subject.rsp_pool(h) }.
          not_to raise_error
      end

      context 'when a remote pool exists already' do
        before :each do
          @initial_name = 'Original Pool Name'
          @pool = Libertree::Model::Pool.create(
            FactoryGirl.attributes_for(:pool, member_id: @member.id, sprung: true, name: @initial_name )
          )
        end

        it 'updates the pool name' do
          h = {
            'username'   => @member.username,
            'id'         => @pool.remote_id,
            'name'       => 'New Pool Name',
          }
          expect { subject.rsp_pool(h) }.
            not_to raise_error

          Libertree::Model::Pool[@pool.id].name.should == 'New Pool Name'
        end
      end
    end
  end
end
