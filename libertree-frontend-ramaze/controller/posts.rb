module Controller
  class Posts < Base
    map '/posts'

    before_all do
      if action.view_value.nil?
        # authentication is handled in single post view (/posts/show/)
        # and on spring display (/s/, /pools/_more/)
        if Ramaze::Current.request.path !~ %r{^(/posts/show/|/s/|/pools/_more/)}
          require_login
        end
        init_locale
      end
    end

    layout do |path|
      if path =~ %r{\b_}
        nil
      else
        :default
      end
    end

    provide(:json, type: 'application/json') { |action,value| value.to_json }

    def _excerpt(post_id)
      @post = Libertree::Model::Post[ post_id.to_i ]
      # caution: only give away this post to strangers if it is
      # visible to the Internet
      if ! @post.v_internet?
        require_login
      end
    end

    def _excerpts( river_id, older_or_newer = 'older', time = Time.now.to_i )
      @river = Libertree::Model::River[ account_id: account.id, id: river_id.to_i ]
      if @river.nil?
        @posts = []
      else
        @river_post_order = session[:river_post_order]
        @posts = @river.posts(
          order_by: @river_post_order,
          limit: 8,
          time: time.to_f,
          newer: ( older_or_newer == 'newer' ),
        ).reverse
      end
    end

    def new
      @view = "post-new"
      @springs = account.member.springs
    end

    def create
      redirect_referrer  if ! request.post?

      text = request['text'].to_s
      # Censor/clean invalid UTF characters
      text.encode!('UTF-16', 'UTF-8', :invalid => :replace, :replace => '?')
      text.encode!('UTF-8', 'UTF-16')

      if text.empty?
        error = _('Post may not be empty.')
        if Ramaze::Current.action.wish == 'json'
          return {
            'success' => false,
            'error' => error,
          }
        else
          flash[:error] = error
          redirect_referrer
        end
      end

      visibility = request['visibility'].to_s

      if text.length > 32
        post = Libertree::Model::Post[
          member_id: account.member.id,
          visibility: visibility,
          text: text
        ]
        if post
          error = _('You already posted that. (%s)' % ago(post.time_created) )
          if Ramaze::Current.action.wish == 'json'
            return {
              'success' => false,
              'error' => error,
            }
          else
            flash[:error] = error
            redirect_referrer
          end
        end
      end

      begin
        post = Libertree::Model::Post.create(
          member_id:  account.member.id,
          visibility: visibility,
          text:       text
        )
      rescue Sequel::DatabaseError => e
        # TODO: test whether this fails when postgresql is running in a non-English locale
        if e.message =~ /value too long/
          error = _('Please submit fewer than 16,000 characters.')
          if Ramaze::Current.action.wish == 'json'
            return {
              'success' => false,
              'error' => error,
            }
          else
            flash[:error] = error
            redirect_referrer
          end
        else
          raise e
        end
      end

      if ! request['spring_ids'].nil?
        spring_ids = Array(request['spring_ids']).map(&:to_i).uniq

        placeholders = ( ['?'] * spring_ids.count ).join(', ')
        springs = Libertree::Model::Pool.where(
          %{
            id IN (#{placeholders})
            AND sprung
            AND member_id = ?
          },
          *spring_ids,
          account.member.id
        )

        springs.each do |spring|
          spring << post
        end
      end

      session[:saved_text]['textarea-post-new'] = nil

      if Ramaze::Current.action.wish == 'json'
        message = _("Successfully posted.")
        river = Libertree::Model::River[ request['river_id'].to_i ]
        if river
          matches_river = river.matches_post?(post)
          if ! matches_river
            message << ' ' + _("Note that your post cannot be seen here because it does not match this river.")
          end
        end
        {
          'success' => true,
          'postId' => post.id,
          'message' => message,
          'matchesRiver' => !! matches_river,
        }
      else
        redirect r(:show, post.id)
      end
    end

    def show(post_id, from_comment_id = nil)
      @view = "single-post-view"
      @post = Libertree::Model::Post.get_full( post_id.to_i )
      if @post.nil?
        respond (render_full "/error_404"), 404
      else
        if ! @post.v_internet?
          require_login
        end

        @subtitle = %{#{@post.member.name_display} - "#{@post.glimpse}"}

        if from_comment_id
          @comment_fetch_options = {
            from_id: from_comment_id.to_i,
          }
        else
          @comment_fetch_options = {
            limit: 50,
          }
        end

        if logged_in?
          @post.mark_as_read_by account
          Libertree::Model::Notification.mark_seen_for_account_and_post  account, @post
        end
      end
    end

    def _read(post_id)
      post = Libertree::Model::Post[post_id.to_i]
      if post
        post.mark_as_read_by account
      end
      ""
    end

    def _unread(post_id)
      post = Libertree::Model::Post[post_id.to_i]
      if post
        post.mark_as_unread_by account
      end
      ""
    end

    def _subscribe(post_id)
      post = Libertree::Model::Post[post_id.to_i]
      if post
        account.subscribe_to post
      end
      ""
    end

    def _unsubscribe(post_id)
      post = Libertree::Model::Post[post_id.to_i]
      if post
        account.unsubscribe_from post
      end
      ""
    end

    def destroy(post_id)
      post = Libertree::Model::Post[post_id.to_i]
      if post && post.member == account.member
        post.delete_cascade
      end

      if Ramaze::Current.action.wish == 'json'
        return { 'success' => true }
      else
        if request.env['HTTP_REFERER'] =~ %r{/posts/show/#{post_id}}
          redirect Home.r(:/)
        else
          redirect_referrer
        end
      end
    end

    def edit(post_id)
      @view = "post-edit"
      @post = Libertree::Model::Post[post_id.to_i]
      redirect_referrer  if @post.nil?
      session[:saved_text]['textarea-post-edit'] = @post.text
    end

    def update
      redirect_referrer  if ! request.post?
      post = Libertree::Model::Post[ request['post_id'].to_i ]
      redirect_referrer  if post.nil? || post.member != account.member

      if ! request.params['cancel']
        text = request['text'].to_s
        # Censor/clean invalid UTF characters
        # TODO: DRY up along with #encode! calls in #create action
        text.encode!('UTF-16', 'UTF-8', :invalid => :replace, :replace => '?')
        text.encode!('UTF-8', 'UTF-16')

        post.revise text, request['visibility'].to_s
        session[:saved_text]['textarea-post-edit'] = nil
      end

      redirect Posts.r(:show, post.id)
    end

    def urls_already_posted
      post = Libertree::Model::Post.urls_already_posted?(request.params['text'].to_s)
      {
        'post_id' => post ? post.id : ''
      }
    end
  end
end
