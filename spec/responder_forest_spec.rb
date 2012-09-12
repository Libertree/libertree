require 'spec_helper'

describe Libertree::Server::Responder::Forest do
  describe 'rsp_forest' do

    context 'with a known requester' do
      before :each do
        @requester = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
      end

      context 'when the requester has INTRODUCEd itself' do
        before :each do
          @s.stub(:challenge_new) { 'abcdefghijklmnopqrstuvwxyz' }
          @s.process %<INTRODUCE { "public_key": #{@requester.public_key.to_json} } >
        end

        context 'when the requester has not AUTHENTICATEd itself' do
          it 'returns ERROR' do
            @s.process 'FOREST { "anything": "anything" }'
            @s.should have_responded_with_code('ERROR')
            @s.response['message'].should =~ /authenticate/i
          end
        end

        context 'when the requester has AUTHENTICATEd itself' do
          before :each do
            @s.process 'AUTHENTICATE { "response": "abcdefghijklmnopqrstuvwxyz" }'
            @s.should have_responded_with_code('OK')
          end

          it 'with a missing name it responds with MISSING PARAMETER' do
            h = {
              'id' => 4,
              'trees' => [
                { 'ip' => '12.34.56.78', },
              ],
            }

            @s.process "FOREST #{h.to_json}"
            @s.should have_responded_with_code('MISSING PARAMETER')
          end

          it 'with a blank name it responds with MISSING PARAMETER' do
            h = {
              'id' => 4,
              'name' => '',
              'trees' => [
                { 'ip' => '12.34.56.78', },
              ],
            }

            @s.process "FOREST #{h.to_json}"
            @s.should have_responded_with_code('MISSING PARAMETER')
          end

          context 'and the forest is not yet known' do
            it 'with valid data it responds with OK' do
              h = {
                'id' => 4,
                'name' => 'New Forest',
                'trees' => [
                  { 'ip' => '12.34.56.78', },
                ],
              }
              @s.process "FOREST #{h.to_json}"
              @s.should have_responded_with_code('OK')

              f = Libertree::Model::Forest[
                origin_server_id: @requester.id,
                remote_id: 4
              ]
              f.name.should == 'New Forest'
              f.trees.count.should == 1
              f.trees[0].ip.should == '12.34.56.78'
            end
          end

          context 'and the forest is known' do
            before :each do
              @forest = Libertree::Model::Forest.create(
                FactoryGirl.attributes_for(
                  :forest,
                  :origin_server_id => @requester.id,
                  :remote_id => 1
                )
              )
            end

            it 'with valid data it responds with OK' do
              h = {
                'id' => @forest.remote_id,
                'name' => 'Different Forest Name',
                'trees' => [
                  { 'ip' => '99.88.77.66', },
                ],
              }
              @s.process "FOREST #{h.to_json}"
              @s.should have_responded_with_code('OK')

              f = Libertree::Model::Forest[id: @forest.id]
              f.name.should == 'Different Forest Name'
              f.trees.count.should == 1
              f.trees[0].ip.should == '99.88.77.66'
            end
          end
        end
      end
    end
  end
end
