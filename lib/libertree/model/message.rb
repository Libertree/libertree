module Libertree
  module Model
    class Message < Sequel::Model(:messages)
      def sender
        @sender ||= Member[self.sender_member_id]
      end
      alias :member :sender

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

        sender_member = Model::Member[ args[:sender_member_id].to_i ]

        recipient_member_ids = Array(args[:recipient_member_ids])
        recipient_member_ids.each do |member_id|
          DB.dbh[ "INSERT INTO message_recipients ( message_id, member_id ) VALUES ( ?, ? )", message.id, member_id.to_i ].get
          m = Member[member_id]
          if m.account
            a = m.account
            a.notify_about  'type' => 'message', 'message_id' => message.id

            # forward via email for those local recipients who requested it
            if a.email && a.settings.forward_dms_via_email
              Libertree::Model::Job.create(
                task: 'email',
                params: {
                  'to'      => a.email,
                  'pubkey'  => a.pubkey,
                  'subject' => '[Libertree] Direct message', # TODO: translate
                  'body'    => "#{sender_member.handle} wrote:\n\n#{args[:text]}"
                }.to_json
              )
            end
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
