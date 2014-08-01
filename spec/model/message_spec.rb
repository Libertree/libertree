require 'spec_helper'

describe Libertree::Model::Message do
  before :all do
    @account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
    @member = @account.member

    local_account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
    @local_member = local_account.member

    remote = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server))
    @remote_member = Libertree::Model::Member.create( FactoryGirl.attributes_for(:member, :server_id => remote.id, :username => 'remote'))
  end

  describe 'create_with_recipients' do
    it 'calls #distribute' do
      expect_any_instance_of(Libertree::Model::Message).to receive(:distribute)
      message = Libertree::Model::Message.
        create_with_recipients({ :sender_member_id => @member.id,
                                 :recipient_member_ids => [@local_member.id],
                                 :text => 'Hello'
                               })
    end

    it 'creates a new local message record' do
      c = Libertree::Model::Message.count
      message = Libertree::Model::Message.
        create_with_recipients({ :sender_member_id => @member.id,
                                 :recipient_member_ids => [@local_member.id],
                                 :text => 'Hello'
                               })
      expect( Libertree::Model::Message.count ).to be(c + 1)
    end

    it 'creates records for local recipients' do
      message = Libertree::Model::Message.
        create_with_recipients({ :sender_member_id => @member.id,
                                 :recipient_member_ids => [@local_member.id],
                                 :text => 'Hello'
                               })
      expect( Libertree::DB.dbh["SELECT * FROM message_recipients WHERE message_id = ?", message.id].count ).not_to be 0
    end
  end

  describe 'distribute' do
    it 'creates a distribution job for each remote server' do
      expect( Libertree::Model::Job ).to receive(:create) do |args|
        expect( args[:task] ).to eq('request:MESSAGE')
      end
      message = Libertree::Model::Message.
        create_with_recipients({ :sender_member_id => @member.id,
                                 :recipient_member_ids => [@remote_member.id],
                                 :text => 'Hello'
                               })
    end
    it 'creates no distribution job for messages to local members' do
      expect( Libertree::Model::Job ).not_to receive(:create)
      message = Libertree::Model::Message.
        create_with_recipients({ :sender_member_id => @member.id,
                                 :recipient_member_ids => [@local_member.id],
                                 :text => 'Hello'
                               })
    end
  end

  before :each do
    Libertree::DB.dbh[ "TRUNCATE messages, message_recipients" ].get
    @message_sent = Libertree::Model::Message.
      create_with_recipients({ :sender_member_id => @member.id,
                               :recipient_member_ids => [@local_member.id, @remote_member.id],
                               :text => 'Hello'
                             })
    @message_received = Libertree::Model::Message.
      create_with_recipients({ :sender_member_id => @local_member.id,
                               :recipient_member_ids => [@member.id, @remote_member.id],
                               :text => 'Bye'
                             })
    @message_remote = Libertree::Model::Message.
      create_with_recipients({ :sender_member_id => @remote_member.id,
                               :recipient_member_ids => [@local_member.id, @member.id],
                               :text => 'Welcome'
                             })
    @message_self = Libertree::Model::Message.
      create_with_recipients({ :sender_member_id => @member.id,
                               :recipient_member_ids => [@member.id],
                               :text => 'dupe'
                             })
    @message_self2 = Libertree::Model::Message.
      create_with_recipients({ :sender_member_id => @member.id,
                               :recipient_member_ids => [@member.id, @local_member.id],
                               :text => 'dupe'
                             })
  end

  describe 'active_local_recipients' do
    it 'returns only local recipients' do
      expect( @message_sent.active_local_participants ).to match_array [@member, @local_member]
      expect( @message_received.active_local_participants ).to match_array [@member, @local_member]
      expect( @message_remote.active_local_participants ).to match_array [@member, @local_member]
      expect( @message_self.active_local_participants ).to match_array [@member]
    end

    it 'does not return recipients who deleted the message' do
      expect( @message_remote.active_local_participants ).to match_array [@member, @local_member]
      @message_remote.delete_for_participant(@member)
      expect( @message_remote.active_local_participants ).to match_array [@local_member]
    end
  end

  describe 'delete_for_participant' do
    it 'destroys the message when the last participant marks the message as deleted' do
      expect( @message_self ).to receive(:delete_cascade)
      @message_self.delete_for_participant(@member)

      expect( @message_remote ).to receive(:delete_cascade)
      @message_remote.delete_for_participant(@member)
      @message_remote.delete_for_participant(@local_member)
    end

    it 'marks the message as deleted for a given recipient (even if it is the same as the sender)' do
      @message_self2.delete_for_participant(@member)
      deleted = Libertree::DB.dbh[ "SELECT deleted FROM message_recipients WHERE message_id = ? AND member_id = ?",
                                 @message_self2.id, @member.id ].single_value
      expect( deleted ).to be_true
    end
  end
end
