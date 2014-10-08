module Controller
  class Springs < Base
    map '/s'

    before_all do
      if action.view_value.nil?
        # NOTE: there is no login required for actions here.
        # Non-members are granted access if the spring is public, but
        # they are only shown internet visible posts
        init_locale
      end
    end

    layout :default

    def index(username, spring_name)
      @view = 'excerpts-view pool'

      account = Libertree::Model::Account[ username: username ]
      if account
        @spring = Libertree::Model::Pool[
          spring_url_name: spring_name,
          sprung: true,
          member_id: account.member.id
        ]
      end

      redirect r(:/)  if @spring.nil?

      posts = if logged_in?
                @spring.posts
              else
                @spring.posts({:public => true})
              end

      render_file("#{Ramaze.options.views[0]}/pools/show.erb",
                  { :pool => @spring,
                    :posts => posts,
                    :member => account.member })
    end
  end
end
