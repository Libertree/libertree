module Libertree
  module Model
    class ChatMessage < M4DBI::Model(:chat_messages)
      # RDBI casting not working with TIMESTAMP WITH TIME ZONE ?
      def time_created
        DateTime.parse self['time_created']
      end

      def sender
        @sender ||= Member[self.from_member_id]
      end

      def recipient
        @recipient ||= Member[self.to_member_id]
      end

      def partner_for(account)
        if account.member == self.sender
          self.recipient
        elsif account.member == self.recipient
          self.sender
        end
      end

      def self.between(account, member, limit = 32)
        return []  if account.nil? || member.nil?

        s(
          %{
            SELECT
              *
            FROM
              chat_messages cm
            WHERE
              from_member_id = ?
              AND to_member_id = ?
              OR
              from_member_id = ?
              AND to_member_id = ?
            ORDER BY
              time_created
            LIMIT #{ [limit.to_i, 0].max }
          },
          account.member.id,
          member.id,
          member.id,
          account.member.id
        )
      end

      def self.mark_seen_between(account, member_id)
        return  if account.nil?

        DB.dbh.u(
          %{
            UPDATE
              chat_messages
            SET
              seen = TRUE
            WHERE
              from_member_id = ?
              AND to_member_id = ?
          },
          member_id.to_i,
          account.member.id
        )
      end
    end
  end
end
