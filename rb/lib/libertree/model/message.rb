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

      # Pass either :recipient_member_usernames or :recipient_member_ids in args.
      def self.create_with_recipients(args)
        message = self.create(
          sender_member_id: args[:sender_member_id],
          text: args[:text]
        )

        if args[:recipient_member_usernames].nil?
          recipient_member_ids = Array(args[:recipient_member_ids])
        else
          recipient_member_ids = []
          Array(args[:recipient_member_usernames]).each do |username|
            account = Account[username: username]
            if account
              recipient_member_ids << account.member.id
            end
          end
        end

        recipient_member_ids.each do |member_id|
          DB.dbh.i  "INSERT INTO message_recipients ( message_id, member_id ) VALUES ( ?, ? )", message.id, member_id.to_i
        end

        message
      end
    end
  end
end
