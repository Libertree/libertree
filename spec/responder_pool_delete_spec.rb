require 'spec_helper'

describe Libertree::Server::Responder::Pool do
  describe 'rsp_pool_delete' do
    include_context 'with an INTRODUCEd and AUTHENTICATEd requester'

    before :each do
      @member = Libertree::Model::Member.create(
        FactoryGirl.attributes_for(:member, :server_id => @requester.id)
      )
      @pool = Libertree::Model::Pool.create(
        FactoryGirl.attributes_for(:pool, member_id: @member.id, sprung: true)
      )
    end

    it 'and a parameter is missing or blank, it responds with MISSING PARAMETER' do
      h = {
        'username' => @member.username,
        'id'       => @pool.remote_id,
      }

      keys = h.keys
      keys.each do |key|
        h_ = h.reject { |k,v| k == key }
        @s.process "POOL-DELETE #{h_.to_json}"
        @s.should have_responded_with_code('MISSING PARAMETER')

        h_ = h.dup
        h_[key] = ''
        @s.process "POOL-DELETE #{h_.to_json}"
        @s.should have_responded_with_code('MISSING PARAMETER')
      end
    end

    it 'with valid data it responds with OK and deletes the local copy' do
      pool_id = @pool.id
      h = {
        'username' => @member.username,
        'id'       => @pool.remote_id,
      }
      @s.process "POOL-DELETE #{h.to_json}"
      @s.should have_responded_with_code('OK')

      Libertree::Model::Pool[pool_id].should be_nil
    end
  end
end
