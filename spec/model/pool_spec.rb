require 'spec_helper'

describe Libertree::Model::Pool do
  it 'Adding a post to a spring notifies the post author' do
    account_poster = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
    member_poster = account_poster.member

    account_springer = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
    member_springer = account_springer.member

    post = Libertree::Model::Post.create(
      FactoryGirl.attributes_for( :post, member_id: member_poster.id, text: 'post to be sprung' )
    )

    spring = Libertree::Model::Pool.create(
      FactoryGirl.attributes_for( :pool, member_id: member_springer.id, name: 'Post Feed', sprung: true )
    )

    expect(account_poster.notifications.count).to eq 0

    spring << post

    account_poster = Libertree::Model::Account[ account_poster.id ]
    expect(account_poster.notifications.count).to eq 1
    expect( account_poster.notifications[0].subject ).to be_kind_of Libertree::Model::Pool
  end
end
