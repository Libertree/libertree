require 'spec_helper'

describe 'Controller::API::V1::Posts', :type => :feature do
  include_context 'rack-test'

  before :each do
    @account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
    @account.api_token = "secrettoken#{@account.id}"
    @account.save
  end

  describe '#create' do
    before :each do
      Libertree::DB.dbh.execute 'TRUNCATE posts CASCADE'
    end

    it 'lets members create new posts' do
      expect(Libertree::Model::Post.all.count).to eq 0

      post '/api/v1/posts/create', 'token' => @account.api_token, 'text' => 'A new post.', 'source' => 'foobar'

      expect(last_response.status).to eq 200
      expect(Libertree::Model::Post.all.count).to eq 1

      response = last_response.body
      json = JSON.parse(response)
      expect(json['success']).to eq true
      expect(json['id']).to be_kind_of Fixnum

      posted = Libertree::Model::Post[ json['id'].to_i ]
      expect(posted).not_to be_nil
      expect(posted.text).to eq 'A new post.'
    end

    it 'removes markup from the source' do
      post '/api/v1/posts/create', 'token' => @account.api_token, 'text' => 'A new post.', 'source' => '[foobar](someurl)'

      expect(last_response.status).to eq 200

      response = last_response.body
      json = JSON.parse(response)
      posted = Libertree::Model::Post[ json['id'].to_i ]
      expect(posted.via).to eq 'foobar'
    end
  end
end
