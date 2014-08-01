require 'factory_girl'

FactoryGirl.define do
  factory :forest, :class => Libertree::Model::Forest do
    sequence(:name) { |n| "forest#{n}" }
  end

  factory :server, :class => Libertree::Model::Server do
    sequence(:ip) { |n| "67.67.#{n/256}.#{n%256}" }
    sequence(:name_given) { |n| "server#{n}" }
  end

  factory :account, :class => Libertree::Model::Account do
    sequence(:username) { |n| "account#{n}" }
    sequence(:password_encrypted) { |n| n }
  end

  factory :member, :class => Libertree::Model::Member do
    sequence(:username) { |n| "member#{n}" }
  end

  factory :profile, :class => Libertree::Model::Profile do
  end

  factory :river, :class => Libertree::Model::River do
    sequence(:label) { |n| "River #{n}" }
  end

  factory :post, :class => Libertree::Model::Post do
    sequence(:text) { |n| "Post #{n}" }
    sequence(:remote_id, 1000)
  end

  factory :comment, :class => Libertree::Model::Comment do
    sequence(:text) { |n| "Comment #{n}" }
    sequence(:remote_id, 1000)
  end

  factory :comment_like, :class => Libertree::Model::CommentLike do
    sequence(:remote_id, 1000)
  end

  factory :post_like, :class => Libertree::Model::PostLike do
    sequence(:remote_id, 1000)
  end

  factory :contact_list, :class => Libertree::Model::ContactList do
    sequence(:name) { |n| "Contact List #{n}" }
  end

  factory :pool, :class => Libertree::Model::Pool do
    sequence(:name) { |n| "Pool #{n}" }
  end

  factory :node, :class => Libertree::Model::Node do
    sequence(:address) { |n| "node-#{n}" }
    access_model 'open'
  end

  factory :node_subscription, :class => Libertree::Model::NodeSubscription do
    sequence(:sub_id) { |n| "sub-id-#{n}" }
    state 'subscribed'
  end
end
