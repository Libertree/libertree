require 'libertree/client'
require 'libertree/model'
require 'libertree/job-processor'
require_relative 'libertree/references'
require 'pony'
require 'net/http'
require 'uri'

module Jobs
  def self.list
    {
      "email"                        => Email,
      "http:avatar"                  => Http::Avatar,
      "http:embed"                   => Http::Embed,
      "post:add-to-rivers"           => Post::AddToRivers,
      "river:refresh"                => River::Refresh,
      "river:refresh-all"            => River::RefreshAll,
      "request:CHAT"                 => Request::CHAT,
      "request:COMMENT"              => Request::COMMENT,
      "request:COMMENT-DELETE"       => Request::COMMENT_DELETE,
      "request:COMMENT-LIKE"         => Request::COMMENT_LIKE,
      "request:COMMENT-LIKE-DELETE"  => Request::COMMENT_LIKE_DELETE,
      "request:FOREST"               => Request::FOREST,
      "request:MEMBER"               => Request::MEMBER,
      "request:MEMBER-DELETE"        => Request::MEMBER_DELETE,
      "request:MESSAGE"              => Request::MESSAGE,
      "request:POOL"                 => Request::POOL,
      "request:POOL-DELETE"          => Request::POOL_DELETE,
      "request:POOL-POST"            => Request::POOL_POST,
      "request:POOL-POST-DELETE"     => Request::POOL_POST_DELETE,
      "request:POST"                 => Request::POST,
      "request:POST-DELETE"          => Request::POST_DELETE,
      "request:POST-LIKE"            => Request::POST_LIKE,
      "request:POST-LIKE-DELETE"     => Request::POST_LIKE_DELETE,
    }
  end

  class Email
    def self.perform(params)
      Pony.mail  to: params['to'], subject: params['subject'], body: params['body']
    end
  end

  module Http
    class Avatar
      def self.options=(opts)
        @avatar_dir = opts['avatar_dir']
        @avatar_url = opts['avatar_url']
      end

      class Redirect < StandardError
        def initialize(url)
          @url = url
        end
        def url
          @url
        end
      end

      def self.fetch(url_string, member, follow_redirect=false)
        uri = URI.parse(url_string)
        if uri.path.empty?
          raise Libertree::JobFailed, "URL contains no path: #{url_string}"
        end

        ext = File.extname(uri.path)
        if ! ['.png', '.gif', '.jpg', '.jpeg'].include?(ext.downcase)
          raise Libertree::JobFailed, "Invalid avatar file type: #{ext}"
        end

        Timeout.timeout(15) do
          http = Net::HTTP.new(uri.host, uri.port)
          if uri.scheme.eql? "https"
            http.use_ssl = true
          end

          resp = http.start {|h| h.get(uri.path)}
          if follow_redirect && [Net::HTTPRedirection, Net::HTTPMovedPermanently].include?(resp.class)
            raise Avatar::Redirect.new(resp['location'])
          end

          if [Net::HTTPSuccess, Net::HTTPOK].include? resp.class
            File.open( "#{@avatar_dir}#{member.id}#{ext}", 'wb' ) { |file|
              file.write(resp.body)
            }
            member.avatar_path = "#{@avatar_url}#{member.id}#{ext}"
          end
        end
      end

      def self.perform(params)
        member = Libertree::Model::Member[ params['member_id'] ]
        raise Libertree::JobFailed, "No member with id #{params['member_id']}"  unless member

        args = [params['avatar_url'], member, true]

        begin
          self.fetch *args
        rescue Avatar::Redirect => e
          args = [e.url, member, false]
          retry
        rescue URI::InvalidURIError, ArgumentError => e
          raise Libertree::JobFailed, "Invalid URI: #{params['avatar_url']}"
        rescue Timeout::Error
          # ignore
        end
      end
    end

    class Embed
      def self.perform(params)
        cached = Libertree::Model::EmbedCache[ url: params['url'] ]
        unless cached
          response = Libertree::Embedder.get(params['url'])
          if response
            Libertree::Model::EmbedCache.create(
              url: params['url'],
              object: response
            )
          end
        end
      end
    end
  end

  module River
    class Refresh
      def self.perform(params)
        river = Libertree::Model::River[ params['river_id'] ]
        if river
          river.refresh_posts( params['n'] || 4096 )
        else
          raise Libertree::JobFailed, "Unknown river_id: #{params['river_id']}"
        end
      end
    end

    class RefreshAll
      def self.perform(params)
        a = Libertree::Model::Account[ params['account_id'] ]
        if a
          a.rivers_not_appended.each { |r|
            r.refresh_posts( params['n'] || 4096 )
          }
        else
          raise Libertree::JobFailed, "Unknown account_id: #{params['account_id']}"
        end
      end
    end
  end

  module Post
    class AddToRivers
      def self.perform(params)
        post = Libertree::Model::Post[ params['post_id'] ]
        if post.nil?
          raise Libertree::JobInvalid, "Unknown post_id: #{params['post_id'].inspect}"
        else
          Libertree::Model::River.each do |river|
            if river.should_contain? post
              Libertree::DB.dbh.i "INSERT INTO river_posts ( river_id, post_id ) VALUES ( ?, ? )", river.id, post.id
            end
          end
        end
      end
    end
  end

  module Request
    def self.init_client_conf(conf)
      key = OpenSSL::PKey::RSA.new File.read(conf['private_key_path'])
      @client_conf =
        {
          :private_key       => key,
          :frontend_url_base => conf['frontend_url_base'],
          :server_name       => conf['server_name'],
          :domain            => conf['domain'],
          :contact           => conf['contact'],
          :log               => conf['log_handle'],
          :log_identifier    => conf['log_identifier'],
          :socket            => conf['relay_socket']
        }
    end

    def self.conf
      @client_conf
    end

    def self.client
      @client ||= Libertree::Client.new(self.conf)
    end

    class RequestJob
      def self.with_tree(server_id, method_name, args)
        server = Libertree::Model::Server[server_id]
        if server.nil?
          raise Libertree::JobFailed, "No server with id #{server_id.inspect}"
        else
          begin
            params = Request.client.send(method_name, *args)
            Request.client.request(server.domain, params)

            # TODO: when the response code is not OK the job should
            # not be marked as successfully completed. Currently, we
            # just ignore the response code from the remote tree,
            # which is very unwise. This defect is also present in the
            # master branch.

          # TODO: should we just catch *all* exceptions?
          rescue Timeout::Error => e
            raise Libertree::RetryJob, "With #{server.domain}: #{e.message}"
          end
        end
      end
    end

    # TODO: Maybe this code is too defensive, checking for nil comment, like post, etc.
    # Removing the checks would clean up the code a bit.
    class CHAT < RequestJob
      def self.perform(params)
        chat_message = Libertree::Model::ChatMessage[ params['chat_message_id'].to_i ]
        if chat_message
          Request.client.req_chat chat_message
        end
      end
    end

    class COMMENT < RequestJob
      def self.perform(params)
        comment = Libertree::Model::Comment[params['comment_id'].to_i]
        return  if comment.nil?

        refs = Libertree::References::extract(comment.text, Request.conf[:frontend_url_base])
        response = with_tree(params['server_id'],
                             :req_comment,
                             [comment, refs])

        if response.xpath("//error/code").text == 'NOT FOUND'
          # Remote didn't recognize the comment author or the referenced post
          # Send the potentially missing data, then retry the comment later.
          case response.xpath("//error/text").text
          when /post/
            if comment.post.local?
              with_tree(params['server_id'],
                        :req_post,
                        [comment.post])
            end
          when /member/
            with_tree(params['server_id'],
                      :req_member,
                      [comment.member])
          else
            if comment.post.local?
              with_tree(params['server_id'],
                        :req_post,
                        [comment.post])
            end
            with_tree(params['server_id'],
                      :req_member,
                      [comment.member])
          end
          raise Libertree::RetryJob, "request associated data first"
        end
      end
    end

    class COMMENT_DELETE < RequestJob
      def self.perform(params)
        with_tree(params['server_id'], :req_comment_delete, [params['comment_id']])
      end
    end

    class COMMENT_LIKE < RequestJob
      def self.perform(params)
        like = Libertree::Model::CommentLike[params['comment_like_id'].to_i]
        if like
          with_tree(params['server_id'], :req_comment_like, [ like ])
        end
      end
    end

    class COMMENT_LIKE_DELETE < RequestJob
      def self.perform(params)
        with_tree(params['server_id'],
                  :req_comment_like_delete,
                  [params['comment_like_id']])
      end
    end

    class FOREST < RequestJob
      def self.perform(params)
        forest = Libertree::Model::Forest[params['forest_id'].to_i]
        with_tree(params['server_id'], :req_forest, [forest])
      end
    end

    class MEMBER < RequestJob
      def self.perform(params)
        member = Libertree::Model::Member[ params['member_id'].to_i ]
        if member
          with_tree(params['server_id'], :req_member, [ member ])
        end
      end
    end

    class MEMBER_DELETE < RequestJob
      def self.perform(params)
        with_tree(params['server_id'], :req_member_delete, [params['username']])
      end
    end

    class MESSAGE < RequestJob
      def self.perform(params)
        message = Libertree::Model::Message[ params['message_id'].to_i ]
        if message
          members = Array(params['recipient_member_ids']).map { |member_id|
            Libertree::Model::Member[member_id.to_i]
          }.compact
          with_tree(params['server_id'], :req_message, [ message, members ])
        end
      end
    end

    class POOL < RequestJob
      def self.perform(params)
        pool = Libertree::Model::Pool[ params['pool_id'].to_i ]
        if pool
          with_tree(params['server_id'], :req_pool, [ pool ])
        end
      end
    end

    class POOL_DELETE < RequestJob
      def self.perform(params)
        pool = Libertree::Model::Pool[ params['pool_id'].to_i ]
        if pool
          with_tree(params['server_id'], :req_pool_delete, [ pool ])
        end
      end
    end

    class POOL_POST < RequestJob
      def self.perform(params)
        pool = Libertree::Model::Pool[params['pool_id'].to_i]
        post = Libertree::Model::Post[params['post_id'].to_i]
        if pool && post
          with_tree(params['server_id'], :req_pool_post, [pool, post])
        end
      end
    end

    class POOL_POST_DELETE < RequestJob
      def self.perform(params)
        pool = Libertree::Model::Pool[params['pool_id'].to_i]
        post = Libertree::Model::Post[params['post_id'].to_i]
        if pool && post
          with_tree(params['server_id'], :req_pool_post_delete, [pool, post])
        end
      end
    end

    class POST < RequestJob
      def self.perform(params)
        post = Libertree::Model::Post[params['post_id'].to_i]
        if post
          refs = Libertree::References::extract(post.text, Request.conf[:frontend_url_base])
          response = with_tree(params['server_id'], :req_post, [post, refs])

          if response.xpath("//error/code").text == 'NOT FOUND'
            # Remote didn't recognize the post author.
            # Send the potentially missing data, then retry the comment later.
            case response.xpath("//error/text").text
            when /member/
              with_tree(params['server_id'], :req_member, [post.member])
            end
            raise Libertree::RetryJob, "request associated data first"
          end
        end
      end
    end

    class POST_DELETE < RequestJob
      def self.perform(params)
        with_tree(params['server_id'], :req_post_delete, [params['post_id']])
      end
    end

    class POST_LIKE < RequestJob
      def self.perform(params)
        like = Libertree::Model::PostLike[ params['post_like_id'].to_i ]
        if like
          with_tree(params['server_id'], :req_post_like, [ like ])
        end
      end
    end

    class POST_LIKE_DELETE < RequestJob
      def self.perform(params)
        with_tree(params['server_id'], :req_post_like_delete, [params['post_like_id']])
      end
    end

  end
end
