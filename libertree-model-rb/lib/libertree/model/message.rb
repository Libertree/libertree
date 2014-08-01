module Libertree
  module Model
    class Message < Sequel::Model(:messages)
      include HasDisplayText

      def sender
        @sender ||= Member[self.sender_member_id]
      end
      alias :member :sender

      def distribute
        trees = self.recipients.reduce(Set.new) { |_trees, recipient|
          if recipient.tree
            _trees << recipient.tree
          end
          _trees
        }
        recipient_ids = self.recipients.map(&:id)

        trees.each do |tree|
          Libertree::Model::Job.create(
            {
              task: 'request:MESSAGE',
              params: {
                'message_id'           => self.id,
                'server_id'            => tree.id,
                'recipient_member_ids' => recipient_ids,
              }.to_json,
            }
          )
        end
      end

      # forward direct message to the given email address of the provided account
      def forward_via_email(account)
        return  unless account
        return  unless self.visible_to?(account)

        Libertree::Model::Job.create(
          task: 'forward-via-email',
          params: {
            'username' => account.username,
            'message_id' => self.id
          }.to_json
        )
      end

      def recipients
        return @recipients  if @recipients
        @recipients = Member.s(
          %{
            SELECT
              m.*
            FROM
                members m
              , message_recipients mr
            WHERE
              mr.message_id = ?
              AND m.id = mr.member_id
          },
          self.id
        )
      end

      # the subset of participants who have not deleted the message
      def active_local_participants
        recipients = Member.s(%{SELECT DISTINCT m.* FROM members m
                                JOIN accounts a ON (a.id = m.account_id)
                                JOIN message_recipients mr ON (m.id = mr.member_id)
                                WHERE mr.message_id = ?
                                AND NOT mr.deleted
                               }, self.id)
        if ! self.deleted && self.sender.local?
          ( recipients + [self.sender] ).uniq
        else
          recipients
        end
      end

      def delete_for_participant(local_member)
        # Delete the message for the local participant only, i.e. by
        # removing the assignment.  If this is the last local
        # participant, delete the whole message.  Note that other
        # recipients / the sender will not see a change in the number
        # of recipients when one of the recipients "deletes" their
        # "copy" of the Message
        participants = active_local_participants

        # not authorised to delete
        return  if participants.empty?

        if participants == [local_member]
          # This is the only member interested in this message; delete it completely.
          self.delete_cascade
        else
          # there are other local members with a pointer to the
          # message. Only mark as deleted for this recipient.
          DB.dbh[ "UPDATE message_recipients SET deleted=true WHERE message_id = ? AND member_id = ?",
                  self.id, local_member.id ].get
          if local_member == self.sender
            self.deleted = true
            self.save
          end
        end
        return true
      end

      def visible_to?(account)
        self.sender == account.member || recipients.include?(account.member)
      end

      def self.create_with_recipients(args)
        message = self.create(
          sender_member_id: args[:sender_member_id],
          remote_id: args[:remote_id],
          text: args[:text]
        )
        sender_member = Model::Member[ args[:sender_member_id].to_i ]

        recipient_member_ids = Array(args[:recipient_member_ids])
        recipient_member_ids.each do |member_id|
          DB.dbh[ "INSERT INTO message_recipients ( message_id, member_id ) VALUES ( ?, ? )", message.id, member_id.to_i ].get
          m = Member[member_id]
          if m.account
            a = m.account
            a.notify_about  'type' => 'message', 'message_id' => message.id
            if a.email && a.settings.forward_dms_via_email
              message.forward_via_email(a)
            end
          end
        end
        message.distribute  if sender_member.local?
        message
      end

      def delete_cascade
        DB.dbh[ "SELECT delete_cascade_message(?)", self.id ].get
      end

      def to_hash
        {
          'id'           => self.id,
          'time_created' => self.time_created,
          'to'           => self.recipients.map(&:name_display),
          'text'         => self.text,
        }
      end
    end
  end
end
