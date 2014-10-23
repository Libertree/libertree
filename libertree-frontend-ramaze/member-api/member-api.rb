require 'grape'
require 'grape-swagger'
require 'libertree/model'

# Dev notes:

# Grape does type coercion and type confirmation for us, so to_i, to_s etc.
# on params are not necessary.

module Libertree
  class ValidateNonEmpty < Grape::Validations::Validator
    def validate_param!(attr_name, params)
      if params[attr_name].strip.empty? || Libertree.plain( params[attr_name] ).empty?
        raise(
          Grape::Exceptions::Validation,
          params: [@scope.full_name(attr_name)],
          message: "#{attr_name.inspect} cannot be empty"
        )
      end
    end
  end

  class URLsAlreadyPostedError < StandardError; end
  class ValidateUrlsNotPosted < Grape::Validations::Validator
    def validate_param!(attr_name, params)
      if Libertree::Model::Post.urls_already_posted?( params[attr_name] )
        raise URLsAlreadyPostedError, "#{attr_name.inspect} cannot contain URLs that have already been posted"
      end
    end
  end

  class ValidatePositiveInteger < Grape::Validations::Validator
    def validate_param!(attr_name, params)
      if params[attr_name].to_i < 1
        raise(
          Grape::Exceptions::Validation,
          params: [@scope.full_name(attr_name)],
          message: "#{attr_name.inspect} must be a positive integer"
        )
      end
    end
  end

  class ParkdownAdapter
    def markdown(text)
      Markdown.new(text).to_html
    end
  end

  class MemberAPI < Grape::API
    helpers do
      def set_account_from_token
        @account = Libertree::Model::Account[ api_token: params['token'] ]
        if @account.nil?
          error! "invalid token", 404
        end

        # Throttling.
        if @account.api_last_used_more_recently_than(Time.now - ($conf['api_min_time_between'] || 5))
          @account = nil
          error! "please try again later and less often", 503
        end

        @account.update(:api_time_last, DateTime.now)
      end
    end

    after_validation do
      if env['REQUEST_PATH'] !~ %r{^/docs}
        set_account_from_token
      end
    end

    # ---------------------

    params do
      requires 'token', type: String, desc: "the authentication token"
    end

    resource 'posts' do
      content_type :v2_posts, 'application/vnd.libertree.v2+json'
      version 'v2', using: :header, vendor: 'libertree.posts'
      formatter :v2_posts, lambda { |object, env| object.to_json }
      format :v2_posts

      desc(
        "Create a new post",
        notes: %{
          Example usage:

              curl -v -X POST -H 'Accept:application/vnd.libertree.v2+json' -d token=542c21f33abcac5c38fa1e32e754e067 -d source=curl -d text='Hello, world!' 'http://nosuchtree.libertreeproject.org/api/posts'
        }
      )

      params do
        requires 'text', type: String, validate_urls_not_posted: true, desc: 'The content of the post to be created'
        requires 'source', type: String, validate_non_empty: true, desc: 'A description of what software or website is originating this post'
        optional 'visibility', type: String, default: 'forest', desc: 'How public this post will be.  "tree", "forest" (default) or "internet"'
      end

      rescue_from URLsAlreadyPostedError do |e|
        Rack::Response.new([e.message], 403).finish
      end

      post do
        post = Libertree::Model::Post.create(
          member_id:  @account.member.id,
          visibility: params['visibility'],
          text:       params['text'],
          via:        Libertree.plain( params['source'] )
        )

        { 'success' => true, 'id' => post.id }
      end

      route_param 'post_id' do
        resource 'comments' do
          content_type :v2_comments, 'application/vnd.libertree.v2+json'
          version 'v2', using: :header, vendor: 'libertree.comments'
          formatter :v2_comments, lambda { |object, env| object.to_json }
          format :v2_comments

          desc(
            "Create a new comment",
            notes: %{
              Example usage:

                  curl -v -X POST -H 'Accept:application/vnd.libertree.v2+json' -d token=542c21f33abcac5c38fa1e32e754e067 -d text='Wow, how interesting!' 'http://nosuchtree.libertreeproject.org/api/posts/123456/comments'
            }
          )

          params do
            requires 'text', type: String, desc: 'The content of the comment to be created'
          end

          post do
            post = Libertree::Model::Post[ params['post_id'].to_i ]
            if post.nil?
              error! "Post not found.", 404
            end

            comment = Libertree::Model::Comment.create(
              member_id:  @account.member.id,
              post_id:    post.id,
              text:       params['text']
            )

            { 'success' => true, 'id' => comment.id }
          end
        end
      end
    end

    params do
      requires 'token', type: String, desc: "the authentication token"
    end

    resource 'invitations' do
      content_type :v2_invitations, 'application/vnd.libertree.v2+json'
      version 'v2', using: :header, vendor: 'libertree.invitations'
      formatter :v2_invitations, lambda { |object, env| object.to_json }
      format :v2_invitations

      desc(
        "Generate a new invitation code",
        notes: %{
          Example usage:

              curl -v -X POST -H 'Accept:application/vnd.libertree.v2+json' -d token=542c21f33abcac5c38fa1e32e754e067 'http://nosuchtree.libertreeproject.org/api/invitations'
        }
      )

      # (No parameters.)

      post do
        invitation = @account.new_invitation
        if invitation.nil?
          { 'success' => false }
        else
          { 'success' => true, 'code' => invitation.code, }
        end
      end
    end

    params do
      requires 'token', type: String, desc: "the authentication token"
    end

    resource 'notifications' do
      content_type :v2_notifications, 'application/vnd.libertree.v2+json'
      version 'v2', using: :header, vendor: 'libertree.notifications'
      formatter :v2_notifications, lambda { |object, env| object.to_json }
      format :v2_notifications

      desc(
        "Retrieve your notifications",
        notes: %{
          Example usage:

              curl -v -X GET -H 'Accept:application/vnd.libertree.v2+json' -d token=542c21f33abcac5c38fa1e32e754e067 -d only-unseen=false 'http://nosuchtree.libertreeproject.org/api/notifications'
        }
      )

      params do
        optional 'only-unseen', type: Boolean, default: true, desc: "whether to retrieve only unseen notifications (default), or all notifications"
        optional 'n', type: Integer, default: 32, validate_positive_integer: true, desc: "the maximum number of notifications to return"
      end

      get do
        n = params['n']

        if params['only-unseen']
          @account.notifications_unseen.take(n).map(&:data)
        else
          @account.notifications.take(n).map(&:data)
        end
      end
    end

    add_swagger_documentation mount_path: '/docs', base_path: '/api', api_version: 'v2', markdown: ParkdownAdapter.new
  end
end
