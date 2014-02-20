require 'bcrypt'
require 'securerandom'

module Libertree
  module Model
    class Account < Sequel::Model(:accounts)

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
          account = Account.where(%{password_reset_code = ? AND NOW() <= password_reset_expiry},
                                  creds['password_reset_code'].to_s).first
          if account
            return account
          end
        end

        account = Account[ username: creds['username'].to_s ]
        if account && account.password == creds['password'].to_s
          account
        end
      end

      def member
        @member ||= Member[ account_id: self.id ]
      end

      def settings
        @settings ||= AccountSettings[ account_id: self.id ]
      end

      def notify_about(data)
        Notification.create(
          account_id: self.id,
          data: data.to_json
        )
      end

      def notifications( limit = 128 )
        @notifications ||= Notification.where(account_id: self.id).reverse_order(:id).limit(limit.to_i).all
      end

      def notifications_unseen
        return @notifications_unseen  if @notifications_unseen
        @notifications_unseen = Notification.where(account_id: self.id, seen: false).order(:id).all
      end

      def num_notifications_unseen
        @num_notifications_unseen ||= Notification.where(account_id: self.id, seen: false).count
      end

      def num_chat_unseen
        ChatMessage.where(to_member_id: self.member.id, seen: false).count
      end

      def num_chat_unseen_from_partner(member)
        ChatMessage.where(from_member_id: member.id,
                          to_member_id: self.member.id,
                          seen: false).count
      end

      def chat_messages_unseen
        ChatMessage.where(to_member_id: self.member.id, seen: false).all
      end

      def chat_partners_current
        Libertree::Model::Member.s(
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
          },
          self.member.id, self.member.id
        )
      end

      def rivers
        River.s("SELECT * FROM rivers WHERE account_id = ? ORDER BY position ASC, id DESC", self.id)
      end

      def rivers_not_appended
        rivers.reject(&:appended_to_all)
      end

      def rivers_appended
        @rivers_appended ||= rivers.find_all(&:appended_to_all)
      end

      def self.create(*args)
        account = super
        member = Member.create( account_id: account.id )
        AccountSettings.create( account_id: account.id )
        account
      end

      def home_river
        River.where(account_id: self.id, home: true).first
      end

      def home_river=(river)
        DB.dbh[ "SELECT account_set_home_river(?,?)", self.id, river.id ].get
      end

      def invitations_not_accepted
        Invitation.where("inviter_account_id = ? AND account_id IS NULL", self.id).order(:id).all
      end

      def new_invitation
        if invitations_not_accepted.count < 5
          Invitation.create( inviter_account_id: self.id )
        end
      end

      def generate_api_token
        self.api_token = SecureRandom.hex(16)
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
        @remote_storage_connection = nil
        @settings = nil
      end

      def admin?
        self.admin
      end

      def subscribe_to(post)
        DB.dbh[ %{SELECT subscribe_account_to_post(?,?)}, self.id, post.id ].get
      end

      def unsubscribe_from(post)
        DB.dbh[ "DELETE FROM post_subscriptions WHERE account_id = ? AND post_id = ?", self.id, post.id ].get
      end

      def subscribed_to?(post)
        DB.dbh[ "SELECT account_subscribed_to_post( ?, ? )", self.id, post.id ].single_value
      end

      def messages( opts = {} )
        limit = opts.fetch(:limit, 30)
        if opts[:newer]
          time_comparator = '>'
        else
          time_comparator = '<'
        end
        time = Time.at( opts.fetch(:time, Time.now.to_f) ).strftime("%Y-%m-%d %H:%M:%S.%6N%z")

        Message.s(
          %{
            SELECT *
            FROM view__messages_sent_and_received
            WHERE member_id = ?
            AND time_created #{time_comparator} ?
            ORDER BY time_created DESC
            LIMIT #{limit}
          },
          self.member.id, time
        )
      end

      # @return [Boolean] true iff password reset was successfully set up
      def self.set_up_password_reset_for(email)
        account = self.where(email: email).first
        if account.nil?
          return false
        end

        account.password_reset_code = SecureRandom.hex(16)
        account.password_reset_expiry = Time.now + 60 * 60
        account.save
        account
      end

      def data_hash
        {
          'account' => {
            'username'           => self.username,
            'time_created'       => self.time_created,
            'email'              => self.email,
            'custom_css'         => self.settings.custom_css,
            'custom_js'          => self.settings.custom_js,
            'custom_link'        => self.settings.custom_link,
            'font_css'           => self.font_css,
            'excerpt_max_height' => self.settings.excerpt_max_height,
            'profile' => {
              'name_display' => self.member.profile.name_display,
              'description'  => self.member.profile.description,
            },

            'rivers'             => self.rivers.map(&:to_hash),
            'posts'              => self.member.posts(limit: 9999999).map(&:to_hash),
            'comments'           => self.member.comments(9999999).map(&:to_hash),
            'messages'           => self.messages(limit: 9999999).map(&:to_hash),
          }
        }
      end

      def online?
        Time.now - time_heartbeat.to_time < 5.01 * 60
      end

      def contact_lists
        ContactList.where(account_id: self.id).all
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
        DB.dbh[ "SELECT account_has_contact_list_by_name_containing_member( ?, ?, ? )",
                self.id, contact_list_name, member.id ].single_value
      end

      def delete_cascade
        handle = self.username
        DB.dbh[ "SELECT delete_cascade_account(?)", self.id ].get

        # distribute deletion of member record
        Libertree::Model::Job.create_for_forests(
          {
            task: 'request:MEMBER-DELETE',
            params: { 'username' => handle, }
          }
        )
      end

      def remote_storage_connection
        @remote_storage_connection ||= RemoteStorageConnection[ account_id: self.id ]
      end
    end
  end
end
