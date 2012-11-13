require 'spec_helper'

describe Libertree::Server::Responder::Member do
  describe 'rsp_member_delete' do
    include_context 'with an INTRODUCEd and AUTHENTICATEd requester'

    it 'with a missing username it responds with MISSING PARAMETER' do
      h = { }
      @s.process "MEMBER-DELETE #{h.to_json}"
      @s.should have_responded_with_code('MISSING PARAMETER')
    end


    it 'with a blank username it responds with MISSING PARAMETER' do
      h = {
        'username' => '',
      }
      @s.process "MEMBER-DELETE #{h.to_json}"
      @s.should have_responded_with_code('MISSING PARAMETER')
    end

    context 'given an existing member' do
      before :each do
        @member = Libertree::Model::Member.create(
          FactoryGirl.attributes_for(:member, :server_id => @requester.id)
        )
      end

      it 'with valid data it responds with OK and deletes the local member record' do
        username = @member.username
        h = {
          'username' => username,
        }
        @s.process "MEMBER-DELETE #{h.to_json}"
        @s.should have_responded_with_code('OK')

        Libertree::Model::Member[ :username => username ].should be_nil
      end
    end
  end
end
