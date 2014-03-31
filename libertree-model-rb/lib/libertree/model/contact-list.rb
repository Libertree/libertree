module Libertree
  module Model
    class ContactList < Sequel::Model(:contact_lists)
      def account
        @account ||= Account[self.account_id]
      end

      def members
        return @members   if @members

        @members = Member.s(
          %{
            SELECT
              m.*
            FROM
                contact_lists_members clm
              , members m
            WHERE
              clm.contact_list_id = ?
              AND m.id = clm.member_id
          },
          self.id
        )
      end

      def members=(arg)
        DB.dbh[ "DELETE FROM contact_lists_members WHERE contact_list_id = ?", self.id ].get
        Array(arg).each do |member_id_s|
          DB.dbh[ "INSERT INTO contact_lists_members ( contact_list_id, member_id ) VALUES ( ?, ? )", self.id, member_id_s.to_i ].get
        end
      end

      def delete_cascade
        DB.dbh[ "SELECT delete_cascade_contact_list(?)", self.id ].get
      end

      def <<(member)
        DB.dbh[
          %{
            INSERT INTO contact_lists_members (
                contact_list_id
              , member_id
            ) SELECT
              ?, ?
            WHERE
              NOT EXISTS (
                SELECT 1
                FROM contact_lists_members
                WHERE
                  contact_list_id = ?
                  AND member_id = ?
              )
          },
          self.id,
          member.id,
          self.id,
          member.id
        ].get
      end
    end
  end
end
