require 'bcrypt'
require 'securerandom'

module Libertree
  module Model
    class Account < M4DBI::Model(:accounts)

      # These two password methods provide a seamless interface to the BCrypted
      # password.  The pseudo-field "password" can be treated like a normal
      # String field for reading and writing.
      def password
        @password ||= BCrypt::Password.new(password_encrypted)
      end

      def password=( new_password )
        @password = BCrypt::Password.create(new_password)
        self.password_encrypted = @password
      end

      # Used by Ramaze::Helper::UserHelper.
      # @return [Account] authenticated account, or nil on failure to authenticate
      def self.authenticate(creds)
        if creds['password_reset_code'].to_s
          account = Account.one_where(
            %{
              password_reset_code = ?
              AND NOW() <= password_reset_expiry
            },
            creds['password_reset_code'].to_s
          )
          if account
            return account
          end
        end

        account = Account[ 'username' => creds['username'].to_s ]
        if account && account.password == creds['password'].to_s
          account
        end
      end

      def member
        @member ||= Member['account_id' => self.id]
      end

      def notify_about(data)
        Notification.create(
          account_id: self.id,
          data: data.to_json
        )
      end

      def notifications( limit = 128 )
        # TODO: prepared statement possible?
        @notifications ||= Notification.s(
          "SELECT * FROM notifications WHERE account_id = ? ORDER BY id DESC LIMIT #{limit.to_i}",
          self.id
        )
      end

      def notifications_unseen
        return @notifications_unseen  if @notifications_unseen

        stm = Notification.prepare(
          "SELECT * FROM notifications WHERE account_id = ? AND seen = FALSE ORDER BY id"
        )
        @notifications_unseen = stm.s(self.id).map { |row| Notification.new row }
        stm.finish
        @notifications_unseen
      end

      def num_notifications_unseen
        @num_notifications_unseen ||= Libertree::DB.dbh.sc("SELECT COUNT(*) FROM notifications WHERE account_id = ? AND seen = FALSE", self.id)
      end

      def num_chat_unseen
        Libertree::DB.dbh.sc("SELECT COUNT(*) FROM chat_messages WHERE to_member_id = ? AND seen = FALSE", self.member.id)
      end

      def num_chat_unseen_from_partner(member)
        Libertree::DB.dbh.sc("SELECT COUNT(*) FROM chat_messages WHERE from_member_id = ? AND to_member_id = ? AND seen = FALSE", member.id, self.member.id)
      end

      def chat_messages_unseen
        Libertree::Model::ChatMessage.s("SELECT * FROM chat_messages WHERE to_member_id = ? AND seen = FALSE", self.member.id)
      end

      def chat_partners_current
        stm = Libertree::Model::Member.prepare(
          %{
            (
              SELECT
                    DISTINCT m.*
                  , EXISTS(
                    SELECT 1
                    FROM chat_messages cm2
                    WHERE
                      cm2.from_member_id = cm.from_member_id
                      AND cm2.to_member_id = cm.to_member_id
                      AND cm2.seen = FALSE
                  ) AS has_unseen_from_other
              FROM
                  chat_messages cm
                , members m
              WHERE
                cm.to_member_id = ?
                AND (
                  cm.seen = FALSE
                  OR cm.time_created > NOW() - '1 hour'::INTERVAL
                )
                AND m.id = cm.from_member_id
            ) UNION (
              SELECT
                    DISTINCT m.*
                  , EXISTS(
                    SELECT 1
                    FROM chat_messages cm2
                    WHERE
                      cm2.from_member_id = cm.to_member_id
                      AND cm2.to_member_id = cm.from_member_id
                      AND cm2.seen = FALSE
                  ) AS has_unseen_from_other
              FROM
                  chat_messages cm
                , members m
              WHERE
                cm.from_member_id = ?
                AND cm.time_created > NOW() - '1 hour'::INTERVAL
                AND m.id = cm.to_member_id
            )
          }
        )
        partners = stm.s(
          self.member.id, self.member.id
        ).map { |row|
          Member.new row
        }
        stm.finish
        partners
      end

      def rivers
        stm = River.prepare("SELECT * FROM rivers WHERE account_id = ? ORDER BY position ASC, id DESC")
        results = stm.s(self.id).map { |row| River.new row }
        stm.finish
        results
      end

      def rivers_not_appended
        rivers.reject(&:appended_to_all)
      end

      def rivers_appended
        @rivers_appended ||= rivers.find_all(&:appended_to_all)
      end

      def theme
        @theme ||= super
      end

      def self.create(*args)
        account = super
        member = Member.create( account_id: account.id )
        account
      end

      def home_river
        stm = River.prepare("SELECT * FROM rivers WHERE account_id = ? AND home = TRUE")
        row = stm.s1(self.id)
        stm.finish
        if row
          River.new row
        end
      end

      def home_river=(river)
        DB.dbh.u "UPDATE rivers SET home = FALSE WHERE account_id = ?", self.id
        DB.dbh.u "UPDATE rivers SET home = TRUE WHERE account_id = ? and id = ?", self.id, river.id
      end

      def invitations_not_accepted
        stm = Invitation.prepare("SELECT * FROM invitations WHERE inviter_account_id = ? AND account_id IS NULL ORDER BY id")
        invitations = stm.s(self.id).map { |row| Invitation.new row }
        stm.finish
        invitations
      end

      def new_invitation
        if invitations_not_accepted.count < 5
          Invitation.create( inviter_account_id: self.id )
        end
      end

      def generate_api_token
        self.api_token = SecureRandom.hex(16)
      end

      # RDBI casting not working with TIMESTAMP WITH TIME ZONE ?
      def api_time_last
        if self['api_time_last']
          DateTime.parse self['api_time_last']
        end
      end

      # @param [Time] time The time to compare to
      # @return [Boolean] whether or not the API was last used by this account
      #                   more recently than the given Time
      def api_last_used_more_recently_than(time)
        self.api_time_last && self.api_time_last.to_time > time
      end

      # Clears some memoized data
      def dirty
        @notifications = nil
        @notifications_unseen = nil
        @num_notifications_unseen = nil
        @rivers_appended = nil
        @theme = nil
        @remote_storage_connection = nil
      end

      def admin?
        self.admin
      end

      def subscribe_to(post)
        DB.dbh.i(
          %{
            INSERT INTO post_subscriptions (
                account_id
              , post_id
            ) SELECT
                ?
              , ?
            WHERE NOT EXISTS(
              SELECT 1
              FROM post_subscriptions ps
              WHERE
                ps.account_id = ?
                AND ps.post_id = ?
            )
          },
          self.id,
          post.id,
          self.id,
          post.id
        )
      end

      def unsubscribe_from(post)
        DB.dbh.d  "DELETE FROM post_subscriptions WHERE account_id = ? AND post_id = ?", self.id, post.id
      end

      def subscribed_to?(post)
        DB.dbh.sc  "SELECT account_subscribed_to_post( ?, ? )", self.id, post.id
      end

      def self.subscribed_to(post)
        stm = prepare(
          %{
            SELECT
              a.*
            FROM
                accounts a
              , post_subscriptions ps
            WHERE
              ps.post_id = ?
              AND a.id = ps.account_id
          }
        )
        accounts = stm.s(post.id).map { |row| self.new row }
        stm.finish
        accounts
      end

      def messages
        stm = Message.prepare(
          %{
            SELECT *
            FROM view__messages_sent_and_received
            WHERE member_id = ?
            ORDER BY id DESC
          }
        )
        records = stm.s(self.member.id).map { |row| Message.new row }
        stm.finish
        records
      end

      # @return [Boolean] true iff password reset was successfully set up
      def self.set_up_password_reset_for(email)
        stm = prepare("SELECT * FROM accounts WHERE email = ?")
        result = stm.s1(email)
        stm.finish
        if result.nil?
          return false
        end

        account = self.new result
        if account
          account.password_reset_code = SecureRandom.hex(16)
          account.password_reset_expiry = Time.now + 60 * 60
          account
        end
      end

      def data_hash
        {
          'account' => {
            'username'           => self.username,
            'time_created'       => self.time_created,
            'email'              => self.email,
            'custom_css'         => self.custom_css,
            'custom_js'          => self.custom_js,
            'custom_link'        => self.custom_link,
            'font_css'           => self.font_css,
            'excerpt_max_height' => self.excerpt_max_height,
            'profile' => {
              'name_display' => self.member.profile.name_display,
              'description'  => self.member.profile.description,
            },

            'rivers'             => self.rivers.map(&:to_hash),
            'posts'              => self.member.posts(limit: 9999999).map(&:to_hash),
            'comments'           => self.member.comments(9999999).map(&:to_hash),
            'messages'           => self.messages.map(&:to_hash),
          }
        }
      end

      # RDBI casting not working with TIMESTAMP WITH TIME ZONE ?
      def time_heartbeat
        DateTime.parse self['time_heartbeat']
      end

      def online?
        Time.now - time_heartbeat.to_time < 5.01 * 60
      end

      def contact_lists
        ContactList.where  account_id: self.id
      end

      # All contacts, from all contact lists
      # TODO: Can we collect this in SQL instead of mapping, etc. in Ruby?
      def contacts
        contact_lists.map { |list| list.members }.flatten.uniq
      end

      def contacts_mutual
        self.contacts.find_all { |c|
          c.account && c.account.contacts.include?(self.member)
        }
      end

      def has_contact_list_by_name_containing_member?(contact_list_name, member)
        DB.dbh.sc  "SELECT account_has_contact_list_by_name_containing_member( ?, ?, ? )",
          self.id, contact_list_name, member.id
      end

      def delete_cascade
        handle = self.username
        DB.dbh.execute "SELECT delete_cascade_account(?)", self.id

        # distribute deletion of member record
        Libertree::Model::Job.create_for_forests(
          {
            task: 'request:MEMBER-DELETE',
            params: { 'username' => handle, }
          }
        )
      end

      def remote_storage_connection
        @remote_storage_connection ||= RemoteStorageConnection['account_id' => self.id]
      end
    end
  end
end
