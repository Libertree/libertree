require 'factory_girl'
require 'libertree/model'

FactoryGirl.define do
  factory :server, :class => Libertree::Model::Server do
    sequence(:domain) { |n|
      "#{n}.libertreeproject.org"
    }
    public_key OpenSSL::PKey::RSA.new(2048, 65537).public_key.to_pem
  end

  factory :account, :class => Libertree::Model::Account do
    sequence(:username) { |n| "account#{n}" }
    password_encrypted '$2a$10$ZnELnOWKT3cUBc4UaFdCxuXU2O.WLOo6lTxLAnbIpbeHnK6bcbp9a'
  end

  factory :member, :class => Libertree::Model::Member do
    sequence(:username) { |n| "member#{n}" }
    server
  end

  factory :post, :class => Libertree::Model::Post do
    sequence(:text) { |n| "Post #{n}" }
  end

  factory :comment, :class => Libertree::Model::Comment do
    sequence(:remote_id, 1000)
  end

  factory :river, :class => Libertree::Model::River do
    sequence(:label) { |n| "River #{n}" }
  end
end
