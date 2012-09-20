require 'spec_helper'

describe Libertree::Server::Responder::Pool do
  describe 'rsp_pool' do
    include_context 'with an INTRODUCEd and AUTHENTICATEd requester'

    context 'and the member is known' do
      before :each do
        @member = Libertree::Model::Member.create(
          FactoryGirl.attributes_for(:member, :server_id => @requester.id)
        )
      end

      it 'with a missing id it responds with MISSING PARAMETER' do
        h = {
          'username'   => @member.username,
          'name'       => 'Pool Name',
        }
        @s.process "POOL #{h.to_json}"
        @s.should have_responded_with_code('MISSING PARAMETER')
      end

      it 'with a blank id it responds with MISSING PARAMETER' do
        h = {
          'username'   => @member.username,
          'id'         => '',
          'name'       => 'Pool Name',
        }
        @s.process "POOL #{h.to_json}"
        @s.should have_responded_with_code('MISSING PARAMETER')
      end

      it "with a member username that isn't found it responds with NOT FOUND" do
        h = {
          'username'   => 'nosuchusername',
          'id'         => 4,
          'name'       => 'Pool Name',
        }
        @s.process "POOL #{h.to_json}"
        @s.should have_responded_with_code('NOT FOUND')
      end

      context 'with valid pool data, and a member that does not belong to the requester' do
        before :each do
          other_server = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
          @member = Libertree::Model::Member.create(
            FactoryGirl.attributes_for(:member, :server_id => other_server.id)
          )
        end

        it 'responds with NOT FOUND' do
          h = {
            'username'   => @member.username,
            'id'         => 4,
            'name'       => 'Pool Name',
          }
          @s.process "POOL #{h.to_json}"
          @s.should have_responded_with_code('NOT FOUND')
        end
      end

      it 'with valid data it responds with OK' do
        h = {
          'username'   => @member.username,
          'id'         => 4,
          'name'       => 'Pool Name',
        }
        @s.process "POOL #{h.to_json}"
        @s.should have_responded_with_code('OK')
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
          @s.process "POOL #{h.to_json}"
          @s.should have_responded_with_code('OK')

          Libertree::Model::Pool[@pool.id].name.should == 'New Pool Name'
        end
      end
    end
  end
end
