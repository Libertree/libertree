require 'json'
require 'em-websocket'

module Libertree
  module WebsocketServer

    $sessions = Hash.new

    def self.start(conf)
      if conf['secure_websocket']
        options = {
          :host => conf['websocket_listen_host'],
          :port => 8080,
          :secure => true,
          :tls_options => {
            :private_key_file => conf['websocket_ssl_private_key'],
            :cert_chain_file => conf['websocket_ssl_cert']
          }
        }
      else
        options = {
          :host => conf['websocket_listen_host'],
          :port => 8080,
        }
      end

      server_blk = lambda do |ws|
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
          end
        end

        ws.onerror do |error|
          $stderr.puts "ERROR: #{error.inspect}"
        end
      end

      EventMachine.run do
        EventMachine::WebSocket.start(options, &server_blk)
        self.monitor_changes
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

    def self.monitor_changes
      EventMachine.add_periodic_timer(2) do
        $sessions.each do |sid,session_data|
          session_data[:sockets].each do |ws,socket_data|
            account = session_data[:account]
            account.dirty

            # Heartbeat every 60 seconds
            if Time.now.strftime("%S") =~ /[0][01]/
              ws.send(
                {
                  'command'   => 'heartbeat',
                  'timestamp' => Time.now.strftime('%H:%M:%S'),
                }.to_json
              )
            end

            # TODO: This is disabled until it is debugged.  Too many false positives.

            # # TODO: The first new post since websocket server start is missed
            # # TODO: The new post text is never updated when it is once set.
            # #       When at first only one new post is detected, but on the
            # #       next iteration 100 new posts are discovered, the hint will
            # #       still say "1 new post".
            # account.rivers_not_appended.each do |river|
              # posts = Libertree::Model::Post.
                # s("SELECT p.* FROM posts p, river_posts rp WHERE rp.river_id = ? AND p.id = rp.post_id AND p.id > ?",
                 # river.id,
                 # socket_data[:last_post_id][river.id])
              # num_posts = posts.count
              # if num_posts > 0
                # post_ids = posts.map(&:id)
                # ws.send(
                  # {
                    # 'command' => 'river-posts',
                    # 'riverId' => river.id,
                    # 'postIds' => post_ids,
                    # # TODO: i18n
                    # 'newPostsMessage' => "#{num_posts} new post#{num_posts == 1 ? '' : 's'}",
                  # }.to_json
                # )
                # socket_data[:last_post_id][river.id] = post_ids.max
              # end
            # end

            notifs = Libertree::Model::Notification.s(
              "SELECT * FROM notifications WHERE id > ? AND account_id = ? ORDER BY id LIMIT 1",
              socket_data[:last_notification_id],
              account.id
            )
            notifs.each do |n|
              ws.send(
                {
                  'command' => 'notification',
                  'id' => n.id,
                  'n' => account.num_notifications_unseen
                }.to_json
              )
              socket_data[:last_notification_id] = n.id
            end

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
    end

  end
end
