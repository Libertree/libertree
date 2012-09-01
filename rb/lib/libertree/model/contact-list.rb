module Libertree
  module Model
    class ContactList < M4DBI::Model(:contact_lists)
      def account
        @account ||= Account[self.account_id]
      end

      def members
        @members ||= Member.prepare(
          %{
            SELECT
              m.*
            FROM
                contact_lists_members clm
              , members m
            WHERE
              clm.contact_list_id = ?
              AND m.id = clm.member_id
          }
        ).s(self.id).map { |row| Member.new row }
      end

      def members=(arg)
        DB.dbh.d  "DELETE FROM contact_lists_members WHERE contact_list_id = ?", self.id
        Array(arg).each do |member_id_s|
          DB.dbh.i  "INSERT INTO contact_lists_members ( contact_list_id, member_id ) VALUES ( ?, ? )", self.id, member_id_s.to_i
        end
      end

      def delete_cascade
        DB.dbh.delete  "DELETE FROM contact_lists_members WHERE contact_list_id = ?", self.id
        self.delete
      end
    end
  end
end
