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

      def member_ids
        Libertree::DB.dbh[:contact_lists_members].
          select(:member_id).
          where(:contact_list_id => self.id).
          all.
          flat_map(&:values)
      end

      def members=(arg)
        DB.dbh.transaction do
          DB.dbh[ "DELETE FROM contact_lists_members WHERE contact_list_id = ?", self.id ].get
          Array(arg).each do |member_id_s|
            DB.dbh[ "INSERT INTO contact_lists_members ( contact_list_id, member_id ) VALUES ( ?, ? )", self.id, member_id_s.to_i ].get
          end
        end
      end

      def delete_cascade
        DB.dbh[ "SELECT delete_cascade_contact_list(?)", self.id ].get
      end

      def <<(member)
        # refuse to add anything that's not a Member
        return  unless member.is_a? Member

        Libertree::DB.dbh.transaction do
          unless self.member_ids.include?(member.id)
            Libertree::DB.dbh[:contact_lists_members].
              insert(contact_list_id: self.id, member_id: member.id)
          end
        end
      end

      # refresh any river containing a reference to this contact list
      def refresh_rivers
        rivers = account.rivers.select do |r|
          vals = r.parsed_query['contact-list'].values.flatten(1)
          ! vals.empty? && vals.map(&:first).include?(self.id)
        end

        # refresh rivers in background jobs
        rivers.each do |river|
          if ! river.appended_to_all
            Libertree::Model::Job.create(
              task: 'river:refresh',
              params: {
                'river_id' => river.id,
              }.to_json
            )
          end
        end
      end

    end
  end
end
