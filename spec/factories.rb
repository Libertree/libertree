require 'factory_girl'

FactoryGirl.define do
  factory :server, :class => Libertree::Model::Server do
    sequence(:ip) { |n|
      m = n / 256  # integer division
      "192.168.#{m}.#{n}"
    }
    public_key OpenSSL::PKey::RSA.new(2048, 65537).public_key.to_pem
    # sequence(:public_key, 0xf000000) { |n| "%08x" % n }
  end

  factory :forest, :class => Libertree::Model::Forest do
    sequence(:name) { |n| "forest#{n}" }
    local_is_member true
  end

  factory :account, :class => Libertree::Model::Account do
    sequence(:username) { |n| "account#{n}" }
    sequence(:password_encrypted) { |n| "%09x" % n }
  end

  factory :member, :class => Libertree::Model::Member do
    sequence(:username) { |n| "member#{n}" }
    server
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

  factory :pool, :class => Libertree::Model::Pool do
    sequence(:name) { |n| "Pool #{n}" }
    sequence(:remote_id, 1000)
  end
end
