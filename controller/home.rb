module Controller
  class Home < Base
    map '/home'
    before_all do
      default_before_filter
    end

    layout do |path|
      if path =~ /_post_icon/
        nil
      else
        :default
      end
    end

    def index(river_id = nil)
      @view = "excerpts-view home"
      @springs = account.member.springs
      @rivers = account.rivers_not_appended
      @river = Libertree::Model::River[ account_id: account.id, id: river_id.to_i ] || account.home_river || @rivers[0]
      @river_post_order = session[:river_post_order]
      if @river
        @posts = @river.posts( order_by: @river_post_order, limit: 16 )
      else
        @posts = []
        @no_rivers = true
      end
    end

    def mark_all_read(river_id=nil)
      if river_id.nil?
        Libertree::Model::Post.mark_all_as_read_by account
      else
        river = Libertree::Model::River[ account_id: account.id, id: river_id.to_i ]
        if river
          river.mark_all_posts_as_read
        else
          flash[:error] = _('River not found.')
        end
      end
      redirect_referrer
    end

    def sort_by_time_updated_overall
      session[:river_post_order] = :comment
      redirect_referrer
    end
    def sort_by_time_created
      session[:river_post_order] = nil
      redirect_referrer
    end
  end
end
