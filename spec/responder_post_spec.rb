require 'spec_helper'

describe Libertree::Server::Responder::Post do
  before :each do
    @s = MockServer.new
  end

  describe 'rsp_post' do

    context 'when the requester has not INTRODUCEd itself' do
      it 'returns ERROR' do
        @s.process 'POST { "anything": "anything" }'
        shouldda_responded_with_code 'ERROR'
        response['message'].should =~ /introduce/i
      end
    end

    context 'with a known requester' do
      before :each do
        @requester = Libertree::Model::Server.create(
          FactoryGirl.attributes_for(:server).merge(
            { :public_key => $test_public_key }
          )
        )
      end

      context 'when the requester has INTRODUCEd itself' do
        before :each do
          @s.stub(:challenge_new) { 'abcdefghijklmnopqrstuvwxyz' }
          @s.process %<INTRODUCE { "public_key": #{$test_public_key.to_json} } >
        end

        context 'when the requester has not AUTHENTICATEd itself' do
          it 'returns ERROR' do
            @s.process 'POST { "anything": "anything" }'
            shouldda_responded_with_code 'ERROR'
            response['message'].should =~ /authenticate/i
          end
        end

        context 'when the requester has AUTHENTICATEd itself' do
          before :each do
            @s.process 'AUTHENTICATE { "response": "abcdefghijklmnopqrstuvwxyz" }'
            shouldda_responded_with_code 'OK'
          end

          context 'and the member is known' do
            before :each do
              @member = Libertree::Model::Member.create(
                FactoryGirl.attributes_for(:member, :server_id => @requester.id)
              )
            end

            it 'with a missing uuid it responds with MISSING PARAMETER' do
              h = {
                'member_uuid' => @member.uuid,
                'public'      => true,
                'text'        => 'A test post.',
              }
              @s.process "POST #{h.to_json}"
              shouldda_responded_with_code 'MISSING PARAMETER'
            end

            it 'with an invalid uuid it responds with ERROR' do
              h = {
                'member_uuid' => @member.uuid,
                'uuid'        => 'invalid uuid',
                'public'      => true,
                'text'        => 'A test post.',
              }
              @s.process "POST #{h.to_json}"
              shouldda_responded_with_code 'ERROR'
            end

            it 'with a blank uuid it responds with MISSING PARAMETER' do
              h = {
                'member_uuid' => @member.uuid,
                'uuid'        => '',
                'public'      => true,
                'text'        => 'A test post.',
              }
              @s.process "POST #{h.to_json}"
              shouldda_responded_with_code 'MISSING PARAMETER'
            end

            context 'with valid post data, and a member that does not belong to the requester' do
              it 'responds with NOT FOUND'
            end

            it 'with valid data it responds with OK' do
              h = {
                'member_uuid' => @member.uuid,
                'uuid'        => 'bcad1067-cfb6-413b-b399-33828cb0c708',
                'public'      => true,
                'text'        => 'A test post.',
              }
              @s.process "POST #{h.to_json}"
              shouldda_responded_with_code 'OK'
            end
          end
        end
      end
    end
  end
end
