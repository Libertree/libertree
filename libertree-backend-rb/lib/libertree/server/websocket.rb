require 'json'
require 'em-websocket'

module Libertree
  module Server
    module Websocket

      $sessions = Hash.new

      def self.run(conf)
        config = if conf['secure_websocket']
          {
            :host => conf['websocket_listen_host'],
            :port => conf['websocket_port'],
            :secure => true,
            :tls_options => {
              :private_key_file => conf['websocket_ssl_private_key'],
              :cert_chain_file => conf['websocket_ssl_cert']
            }
          }
        else
          {
            :host => conf['websocket_listen_host'],
            :port => conf['websocket_port'],
          }
        end

        EventMachine::WebSocket.run(config) {|ws| self.server(ws)}
        [:notifications, :chat_messages, :comments].each do |channel|
          EventMachine.defer do
            Libertree::DB.dbh.listen(channel, :loop => true) {|channel| self.handle(channel)}
          end
        end
        self.heartbeat
      end

      def self.server(ws)
        ws.onopen do
        end

        ws.onclose do
          $sessions.each do |sid,session_data|
            session_data[:sockets].delete ws
          end
        end

        ws.onmessage do |json_data|
          begin
            self.onmessage ws, JSON.parse(json_data)
          rescue Exception => e
            $stderr.puts e.message + "\n" + e.backtrace.join("\n\t")
            raise e
          end
        end

        ws.onerror do |error|
          $stderr.puts "ERROR: #{error.inspect}"
        end
      end

      def self.onmessage(ws, data)
        sid = data['sid']
        session_account = Libertree::Model::SessionAccount[sid: sid]
        if session_account.nil?
          puts "Unrecognized session: #{sid}"
          return
        end

        $sessions[sid] ||= {
          sockets: Hash.new,
          account: session_account.account,
        }

        $sessions[sid][:sockets][ws] ||= {
          last_post_id: session_account.account.rivers_not_appended.reduce({}) {|acc, river|
            last_post = river.posts({:limit => 1}).first
            if last_post
              acc[river.id] = last_post.id
            else
              acc[river.id] = Libertree::DB.dbh[ "SELECT MAX(id) FROM posts" ].single_value
            end
            acc
          },
          last_notification_id: Libertree::DB.dbh[ "SELECT MAX(id) FROM notifications WHERE account_id = ?", session_account.account.id ].single_value,
          last_comment_id: Libertree::DB.dbh[ "SELECT MAX(id) FROM comments" ].single_value,
          last_chat_message_id: Libertree::DB.dbh[
            "SELECT MAX(id) FROM chat_messages WHERE to_member_id = ? OR from_member_id = ?",
            session_account.account.member.id,
            session_account.account.member.id
          ].single_value,
        }
      end

      def self.heartbeat
        EventMachine.add_periodic_timer(60) do
          $sessions.each do |sid,session_data|
            session_data[:sockets].each do |ws,socket_data|

              ws.send({ 'command'   => 'heartbeat',
                        'timestamp' => Time.now.strftime('%H:%M:%S'),
                      }.to_json)
            end
          end
        end
      end

      def self.handle(channel)
        case channel
        when 'notifications'
          self.handle_notifications
        when 'chat_messages'
          self.handle_chat_messages
        when 'comments'
          self.handle_comments
        else
          $stderr.puts "No handler for channel: #{channel}"
        end
      end

      def self.handle_notifications
        $sessions.each do |sid,session_data|
          session_data[:sockets].each do |ws,socket_data|
            account = session_data[:account]
            account.dirty

            notifs = Libertree::Model::Notification.s(
              "SELECT * FROM notifications WHERE id > ? AND account_id = ? ORDER BY id LIMIT 1",
              socket_data[:last_notification_id],
              account.id
            )
            notifs.each do |n|
              ws.send({ 'command' => 'notification',
                        'id' => n.id,
                        'n' => account.num_notifications_unseen
                      }.to_json)
              socket_data[:last_notification_id] = n.id
            end
          end
        end
      end

      def self.handle_chat_messages
        $sessions.each do |sid,session_data|
          session_data[:sockets].each do |ws,socket_data|
            account = session_data[:account]
            account.dirty

            chat_messages = Libertree::Model::ChatMessage.s(
              "SELECT * FROM chat_messages WHERE id > ? AND ( to_member_id = ? OR from_member_id = ? ) ORDER BY id",
              socket_data[:last_chat_message_id],
              account.member.id,
              account.member.id
            )

            chat_messages.each do |cm|
              partner = cm.partner_for(account)
              ws.send(
                {
                  'command'             => 'chat-message',
                  'id'                  => cm.id,
                  'partnerMemberId'     => partner.id,
                  'numUnseen'           => account.num_chat_unseen,
                  'numUnseenForPartner' => account.num_chat_unseen_from_partner(partner),
                }.to_json
              )
              socket_data[:last_chat_message_id] = cm.id
            end
          end
        end
      end

      def self.handle_comments
        $sessions.each do |sid,session_data|
          session_data[:sockets].each do |ws,socket_data|
            account = session_data[:account]
            account.dirty

            comments = Libertree::Model::Comment.comments_since_id( socket_data[:last_comment_id] )
            comments.each do |c|
              ws.send(
                {
                  'command'   => 'comment',
                  'commentId' => c.id,
                  'postId'    => c.post.id,
                }.to_json
              )
              socket_data[:last_comment_id] = c.id
            end
          end
        end
      end
    end
  end
end
