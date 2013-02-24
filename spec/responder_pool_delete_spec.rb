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

  describe 'rsp_pool_delete' do
    include_context 'requester in a forest'

    before :each do
      @member = Libertree::Model::Member.create(
        FactoryGirl.attributes_for(:member, :server_id => @requester.id)
      )
      @pool = Libertree::Model::Pool.create(
        FactoryGirl.attributes_for(:pool, member_id: @member.id, sprung: true)
      )
      subject.instance_variable_set(:@server, @requester)
    end

    it 'raises MissingParameter when a parameter is missing or blank' do
      h = {
        'username' => @member.username,
        'id'       => @pool.remote_id,
      }

      keys = h.keys
      keys.each do |key|
        h_ = h.reject { |k,v| k == key }
        expect { subject.rsp_pool_delete(h_) }.
          to raise_error( Libertree::Server::MissingParameter )

        h_ = h.dup
        h_[key] = ''
        expect { subject.rsp_pool_delete(h_) }.
          to raise_error( Libertree::Server::MissingParameter )
      end
    end

    it 'deletes the local copy and raises no errors with valid data' do
      pool_id = @pool.id
      h = {
        'username' => @member.username,
        'id'       => @pool.remote_id,
      }
      expect { subject.rsp_pool_delete(h) }.
        not_to raise_error

      Libertree::Model::Pool[pool_id].should be_nil
    end
  end
end
