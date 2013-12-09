require 'spec_helper'
require_relative '../lib/jobs'

describe Jobs::Email, "#perform" do
  it 'sends and email' do

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
