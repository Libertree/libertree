require 'spec_helper'
require 'libertree/client'
require_relative '../lib/jobs'

describe Jobs do

  describe Jobs::River do
    describe 'Refresh#perform' do
      it 'calls #refresh_posts on the identified river' do
        pending
      end
    end

    describe 'RefreshAll#perform' do
      it "calls #refresh_posts on all of the account's rivers" do
        pending
      end
    end
  end

  describe Jobs::Request do
    describe 'CHAT#perform' do
      pending
    end

    describe 'COMMENT#perform' do
      pending
    end

    describe 'COMMENT_DELETE#perform' do
      pending
    end

    describe 'COMMENT_LIKE#perform' do
      pending
    end

    describe 'COMMENT_LIKE_DELETE#perform' do
      pending
    end

    describe 'FOREST#perform' do
      pending
    end

    describe 'MEMBER#perform' do
      pending
    end

    describe 'MEMBER_DELETE#perform' do
      pending
    end

    describe 'MESSAGE#perform' do
      pending
    end

    describe 'POOL#perform' do
      pending
    end

    describe 'POOL_DELETE#perform' do
      pending
    end

    describe 'POOL_POST#perform' do
      pending
    end

    describe 'POOL_POST_DELETE#perform' do
      pending
    end

    describe 'POST#perform' do
      pending
    end

    describe 'POST_DELETE#perform' do
      pending
    end

    describe 'POST_LIKE#perform' do
      pending
    end

    describe 'POST_LIKE_DELETE#perform' do
      pending
    end
  end
end
