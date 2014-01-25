require 'spec_helper'

describe Libertree::Model::Member do
  describe '#after_create' do
    it 'creates a distribution job' do
      Libertree::Model::Job.should_receive(:create_for_forests)
      @account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
      @member = @account.member
    end
  end

  describe '#after_update' do
    context 'with existing member' do
      before :each do
        @account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
        @member = @account.member
      end

      it 'creates a distribution job whenever the member record is updated' do
        Libertree::Model::Job.should_receive(:create_for_forests)
        @member.avatar_path = "/new/path"
      end
    end
  end
end
