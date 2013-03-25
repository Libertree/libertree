module Libertree
  module Model
    class Message < M4DBI::Model(:messages)
      # RDBI casting not working with TIMESTAMP WITH TIME ZONE ?
      def time_created
        DateTime.parse self['time_created']
      end

      def sender
        @sender ||= Member[self.sender_member_id]
      end
      alias :member :sender

      def recipients
        @recipients ||= Member.prepare(
          %{
            SELECT
              m.*
            FROM
                members m
              , message_recipients mr
            WHERE
              mr.message_id = ?
              AND m.id = mr.member_id
          }
        ).s(self.id).
          map { |row| Member.new row }
      end

      def visible_to?(account)
        self.sender == account.member || recipients.include?(account.member)
      end

      def glimpse( length = 60 )
        if self.text.length <= length
          self.text
        else
          self.text[0...length] + '...'
        end
      end

      def self.create_with_recipients(args)
        message = self.create(
          sender_member_id: args[:sender_member_id],
          text: args[:text]
        )

        recipient_member_ids = Array(args[:recipient_member_ids])
        recipient_member_ids.each do |member_id|
          DB.dbh.i  "INSERT INTO message_recipients ( message_id, member_id ) VALUES ( ?, ? )", message.id, member_id.to_i
          m = Member[member_id]
          if m.account
            m.account.notify_about  'type' => 'message', 'message_id' => message.id
          end
        end

        message
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
