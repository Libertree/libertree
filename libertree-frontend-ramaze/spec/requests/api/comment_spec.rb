require 'spec_helper'

describe 'Controller::API::V1::Comments', :type => :feature do
  include_context 'rack-test'

  before :each do
    @account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
    @account.api_token = "secrettoken#{@account.id}"
    @account.save
  end

  describe '#create' do
    before :each do
      Libertree::DB.dbh.execute 'TRUNCATE posts CASCADE'
      Libertree::DB.dbh.execute 'TRUNCATE comments CASCADE'
      @post = Libertree::Model::Post.create( FactoryGirl.attributes_for(:post, :member_id => @account.member.id) )
    end

    it 'lets members create new comments on existing posts' do
      expect(@post.comments.count).to eq 0

      post '/api/v1/comments/create',
           'token' => @account.api_token,
           'text' => 'A new comment.',
           'post_id' => @post.id

      expect(last_response.status).to eq 200
      expect(@post.comments.count).to eq 1

      response = last_response.body
      json = JSON.parse(response)
      expect(json['success']).to eq true
      expect(json['id']).to be_kind_of Fixnum

      posted = Libertree::Model::Comment[ json['id'].to_i ]
      expect(posted).not_to be_nil
      expect(posted.text).to eq 'A new comment.'
    end

  end
end
