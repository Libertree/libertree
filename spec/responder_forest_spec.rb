require 'spec_helper'

describe Libertree::Server::Responder::Forest do
  let(:subject_class) { Class.new }
  let(:subject) { subject_class.new }

  before :each do
    subject_class.class_eval {
      include Libertree::Server::Responder::Helper
      include Libertree::Server::Responder::Forest
    }
  end

  describe 'rsp_forest' do
    include_context 'requester in a forest'
    before :each do
      subject.instance_variable_set(:@remote_tree, @requester)
    end

    it 'raises MissingParameter with a missing name' do
      h = {
        'id' => 4,
        'trees' => [
          { 'ip' => '12.34.56.78', },
        ],
      }
      expect { subject.rsp_forest(h) }.
        to raise_error( Libertree::Server::MissingParameter )
    end

    it 'raises MissingParameter with a blank name' do
      h = {
        'id' => 4,
        'name' => '',
        'trees' => [
          { 'ip' => '12.34.56.78', },
        ],
      }

      expect { subject.rsp_forest(h) }.
        to raise_error( Libertree::Server::MissingParameter )
    end

    context 'and the forest is not yet known' do
      it 'raises no errors with valid data' do
        h = {
          'id' => 4,
          'name' => 'New Forest',
          'trees' => [
            { 'ip' => '12.34.56.78', },
          ],
        }
        expect { subject.rsp_forest(h) }.
          not_to raise_error

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

      it 'raises no errors with valid data' do
        h = {
          'id' => @forest.remote_id,
          'name' => 'Different Forest Name',
          'trees' => [
            { 'ip' => '99.88.77.66', },
          ],
        }
        expect { subject.rsp_forest(h) }.
          not_to raise_error

        f = Libertree::Model::Forest[id: @forest.id]
        f.name.should == 'Different Forest Name'
        f.trees.count.should == 1
        f.trees[0].ip.should == '99.88.77.66'
      end
    end
  end
end
