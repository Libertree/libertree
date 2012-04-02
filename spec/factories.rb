require 'factory_girl'

FactoryGirl.define do
  factory :server, :class => Libertree::Model::Server do
    sequence(:ip) { |n|
      m = n / 256  # integer division
      "192.168.#{m}.#{n}"
    }
    public_key RCrypt.generate_key_pair[:public]
    # sequence(:public_key, 0xf000000) { |n| "%08x" % n }
  end

  factory :member, :class => Libertree::Model::Member do
    sequence(:username) { |n| "member#{n}" }
    server
  end
end
