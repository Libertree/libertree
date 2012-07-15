require 'libertree/client'
require 'libertree/model'
require 'pony'

module Helper
  # TODO: find a place for this
  def init_mailer(conf)
    Pony.options = {
      :via => :smtp,
      :via_options => {
        :address              => conf['smtp']['host'],
        :port                 => conf['smtp']['port'],
        :user_name            => conf['smtp']['username'],
        :password             => conf['smtp']['password'],
        :authentication       => conf['smtp']['authentication'],
        :domain               => conf['smtp']['helo_domain'],
        :enable_starttls_auto => conf['smtp']['starttls_auto'],
      },
      :from => conf['smtp']['from_address'],
    }
  end

  def lt_client(remote_host)
    key = OpenSSL::PKey::RSA.new File.read(@conf['private_key_path'])
    c = Libertree::Client.new(
      public_key: key.public_key,
      private_key: key,
      avatar_url_base: @conf['avatar_url_base'],
      server_ip: @conf['ip_public'],
      server_name: @conf['server_name'],
      log: @log,
      log_identifier: @log_identifier
    )

    if c
      c.connect remote_host
      if block_given?
        yield c
        c.close
      end
    end

    c
  end

  def with_tree(server_id)
    server = Libertree::Model::Server[server_id]
    if server.nil?
      log_error "No server with id #{server_id.inspect}"
    else
      begin
        lt_client(server.ip) do |client|
          yield client
        end
      rescue Errno::ETIMEDOUT, Errno::ECONNREFUSED => e
        log_error "With #{server.name_display} (#{server.ip}): #{e.message}"
      end
    end
  end
end

module Jobs
  class Email
    def self.perform(params)
      Pony.mail  to: params['to'], subject: params['subject'], body: params['body']
      return true
    end
  end

  module River
    class Refresh_all
      def self.perform(params)
        a = Libertree::Model::Account[ params['account_id'] ]
        if a
          a.rivers_not_appended.each(&:refresh_posts)
        else
          log_error "Unknown account_id: #{params['account_id']}"
        end
        return true
      end
    end
  end


  module Request

    # TODO: Maybe this code is too defensive, checking for nil comment, like post, etc.
    # Removing the checks would clean up the code a bit.
    class CHAT
      def self.perform(params)
        chat_message = Libertree::Model::ChatMessage[ params['chat_message_id'].to_i ]
        if chat_message
          Helper::with_tree(params['server_id']) do |tree|
            tree.req_chat chat_message
          end
          return true
        end
      end
    end

    class COMMENT
      def self.perform(params)
        comment = Libertree::Model::Comment[params['comment_id'].to_i]
        retry_later = false
        if comment
          Helper::with_tree(params['server_id']) do |tree|
            response = tree.req_comment(comment)
            if response['code'] == 'NOT FOUND'
              # Remote didn't recognize the comment author or the referenced post
              # Send the potentially missing data, then retry the comment later.
              retry_later = true
              case response['message']
              when /post/
                if comment.post.local?
                  tree.req_post comment.post
                end
              when /member/
                tree.req_member comment.member
              else
                if comment.post.local?
                  tree.req_post comment.post
                end
                tree.req_member comment.member
              end
            end
          end
        end

        # return true means: completed
        return ! retry_later
      end
    end

    class COMMENT_DELETE
      def self.perform(params)
        Helper::with_tree(params['server_id']) do |tree|
          tree.req_comment_delete params['comment_id']
        end
        return true
      end
    end

    class COMMENT_LIKE
      def self.perform(params)
        like = Libertree::Model::CommentLike[params['comment_like_id'].to_i]
        if like
          Helper::with_tree(params['server_id']) do |tree|
            tree.req_comment_like like
          end
        end
        return true
      end
    end

    class COMMENT_LIKE_DELETE
      def self.perform(params)
        Helper::with_tree(params['server_id']) do |tree|
          tree.req_comment_like_delete params['comment_like_id']
        end
        return true
      end
    end

    class FOREST
      def self.perform(params)
        forest = Libertree::Model::Forest[params['forest_id'].to_i]
        Helper::with_tree(params['server_id']) do |tree|
          tree.req_forest forest
        end
        return true
      end
    end

    class MEMBER
      def self.perform(params)
        member = Libertree::Model::Member[params['member_id'].to_i]
        if member
          Helper::with_tree(params['server_id']) do |tree|
            tree.req_member member
          end
        end
        return true
      end
    end

    class MESSAGE
      def self.perform(params)
        message = Libertree::Model::Message[params['message_id'].to_i]
        if message
          Helper::with_tree(params['server_id']) do |tree|
            tree.req_message message, params['recipient_usernames']
          end
        end
        return true
      end
    end

    class POST
      def self.perform(params)
        post = Libertree::Model::Post[params['post_id'].to_i]
        if post
          Helper::with_tree(params['server_id']) do |tree|
            tree.req_post post
          end
        end
        return true
      end
    end

    class POST_DELETE
      def self.perform(params)
        Helper::with_tree(params['server_id']) do |tree|
          tree.req_post_delete params['post_id']
        end
        return true
      end
    end

    class POST_LIKE
      def self.perform(params)
        like = Libertree::Model::PostLike[params['post_like_id'].to_i]
        if like
          Helper::with_tree(params['server_id']) do |tree|
            tree.req_post_like like
          end
        end
        return true
      end
    end

    class POST_LIKE_DELETE
      def self.perform(params)
        Helper::with_tree(params['server_id']) do |tree|
          tree.req_post_like_delete params['post_like_id']
        end
        return true
      end
    end

  end
end
