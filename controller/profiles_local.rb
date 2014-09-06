module Controller
  class ProfilesLocal < Base
    map '/p'
    before_all do
      default_before_filter
    end

    layout do |path|
      if path =~ %r{^_}
        nil
      else
        :default
      end
    end

    def index( username, post_string=nil )
      @view = "excerpts-view profile"
      return  if username.nil?

      account = Libertree::Model::Account[ username: username ]
      if account.nil?
        redirect_referrer
      end

      # /p/:username/:post_id-plus-some-optional-text
      # TODO: is it a bad idea to completely ignore the text part of the URL?
      # This allows someone to create many URLs with different text parts that
      # all point to the same post.
      if post_string
        match = post_string.match(%r{(?<post_id>\d+)-?})
        if match.nil?
          redirect_referrer
        end
        post_id = match['post_id'].to_i
        post = Libertree::Model::Post[ id: post_id, member_id: account.member.id ]

        redirect Posts.r(:show, post.id)
      else
        @member = account.member
        @profile = @member.profile
        @posts = @member.posts
      end
    end
  end
end

