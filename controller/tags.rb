module Controller
  class Tags < Base
    map '/tags'
    before_all do
      default_before_filter
    end

    layout do |path|
      if path =~ /^_more/
        nil
      else
        :default
      end
    end

    def index(tag_string)
      redirect_referrer  if tag_string.nil?
      @view = "excerpts-view tags"
      @tags = tag_string.split(' ').map(&:downcase)
      @tag_string = tag_string
      @tag_list = @tags.map{|tag| "##{tag}"}.join(' ')
      @tag_river_query = @tags.map{|tag| "%23#{tag}"}.join('%20')
      @rivers = account.rivers_not_appended

      # TODO: better name for river_post_order?
      @post_order = session[:river_post_order]
      @posts = Libertree::Model::Post.with_tag(
        tag: @tags,
        order_by: @post_order,
        limit: 16,
      ).reverse
    end

    def _more( tags, older_or_newer = 'older', time = Time.now.to_i )
      tags = tags.split(' ').map(&:downcase)
      @posts = Libertree::Model::Post.with_tag(
        tag: tags,
        order_by: session[:river_post_order],
        limit: 8,
        time: time.to_f,
        newer: ( older_or_newer == 'newer' ),
      )
      render_file "#{Ramaze.options.views[0]}/posts/_excerpts.xhtml"
    end
  end
end
