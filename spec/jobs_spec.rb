require 'spec_helper'
require 'tmpdir'
require_relative '../lib/jobs'

describe Jobs::Email, "#perform" do
  it 'sends an email' do

    Mail.defaults do
      delivery_method :test
    end

    Mail::TestMailer.deliveries.clear
    Mail::TestMailer.deliveries.length.should == 0

    Jobs::Email.from = "sender@localhost"
    Jobs::Email.perform({ 'to'      => 'test@localhost',
                          'subject' => 'testing',
                          'body'    => 'this is a test' })
    Mail::TestMailer.deliveries.length.should == 1
  end
end

describe Jobs::Http::Avatar, "#perform" do
  before :each do
    account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
    @member = account.member
    Jobs::Http::Avatar.options = {
      'avatar_dir' => Dir.tmpdir
    }
  end

  it 'raises JobInvalid with an invalid URL' do
    params = {
      'member_id' => @member.id,
      'avatar_url' => "not a valid URL"
    }
    expect { Jobs::Http::Avatar.perform(params) }.to raise_exception(Libertree::JobInvalid)
  end
end
