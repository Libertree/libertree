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

      def recipients
        @recipients ||= Member.s(
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

      def self.create_with_recipients(args)
        message = self.create(
          sender_member_id: args[:sender_member_id],
          text: args[:text]
        )
        Array(args[:recipient_member_ids]).each do |member_id|
          DB.dbh.i  "INSERT INTO message_recipients ( message_id, member_id ) VALUES ( ?, ? )", message.id, member_id.to_i
        end
        message
      end
    end
  end
end
