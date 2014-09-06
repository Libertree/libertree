module Controller
  class Rivers < Base
    map '/rivers'
    before_all do
      default_before_filter
    end

    layout do |path|
      if path =~ %r{\b_}
        nil
      else
        :default
      end
    end

    def index
      @view = "rivers"
      @rivers = account.rivers_not_appended
      @rivers_global = account.rivers_appended
    end

    def new
      @view = "rivers"
    end

    def _create_tutorial_river
      return  if ! request.post?

      begin
        query = request['query'].to_s.strip
        if ! query.empty?
          query.gsub!( /[,;']/, '' )
          Libertree::Model::River.create(
            account_id: account.id,
            label: s_('tutorial-river-label|My interests'),
            query: query,
            home: true,
          )
        end
        { 'status' => 'success' }.to_json
      rescue
        {
          'status' => 'error',
          'msg'    => s_('tutorial|An error occurred while trying to create a river.')
        }.to_json
      end
    end

    def _create_default_rivers
      return  if ! request.post?
      failed = []

      request['rivers'].each_pair do |i, river|
        begin
          Libertree::Model::River.create(
            account_id: account.id,
            label: river['label'].to_s,
            query: river['query'].to_s,
          )
        rescue
          failed << river['label']
          next
        end
      end

      # TODO: report failures instead of ignoring them
      { 'status' => 'success' }.to_json
    end

    def create
      redirect_referrer  if ! request.post?

      begin
        river = Libertree::Model::River.create(
          account_id: account.id,
          label: request['label'].to_s,
          query: request['query'].to_s,
          appended_to_all: !! request['appended_to_all']
        )
      rescue Sequel::UniqueConstraintViolation => e
        if e.message =~ /rivers_account_id_key/
          flash[:error] = _('You already have a river for that.')
          redirect_referrer
        else raise e end
      rescue Sequel::CheckConstraintViolation => e
        if e.message =~ /rivers_label_check/
          flash[:error] = _('Please input a valid label for this river.')
          redirect_referrer
        else raise e end
      end

      if river.appended_to_all
        redirect r(:/)
      else
        redirect Home.r(:/, river.id)
      end
    end

    def destroy(river_id)
      river = Libertree::Model::River[ account_id: account.id, id: river_id.to_i ]
      if river
        river.delete_cascade
      end

      redirect_referrer
    end

    def edit(river_id)
      @view = "rivers"
      @river = Libertree::Model::River[ account_id: account.id, id: river_id.to_i ]
      redirect_referrer  if @river.nil?
    end

    def update(river_id)
      redirect Home.r(:/)  if ! request.post?

      @river = Libertree::Model::River[ account_id: account.id, id: river_id.to_i ]
      redirect Home.r(:/)  if @river.nil?

      begin
        @river.revise request.params
      rescue Sequel::CheckConstraintViolation => e
        if e.message =~ /rivers_label_check/
          flash[:error] = _('Please input a valid label for this river.')
          redirect_referrer
        else raise e end
      rescue Sequel::UniqueConstraintViolation => e
        if e.message =~ /rivers_account_id_key/
          flash[:error] = _('You already have a river with this exact query.')
          redirect_referrer
        else raise e end
      end

      redirect r(:/)
    end

    def ensure_exists(query, label = nil)
      river = Libertree::Model::River.find_or_create(
        account_id: account.id,
        label: label || query,
        query: query
      )

      redirect "/home/#{river.id}"
    end

    def position(from_river_id, before_river_id = nil)
      rivers = account.rivers
      from_river = Libertree::Model::River[ from_river_id.to_i ]

      if from_river
        extracted = rivers.delete(from_river)
        before_river = Libertree::Model::River[ before_river_id.to_i ]

        if before_river.nil?
          rivers << extracted
        else
          rivers.insert( rivers.index(before_river), extracted )
        end

        rivers.each_with_index do |r,i|
          r.position = i
        end
      end

      redirect Home.r(:/)
    end

    def set_home(river_id)
      account.home_river = Libertree::Model::River[ account_id: account.id, id: river_id.to_i ]
      account.save
      redirect_referrer
    end

    def add_spring(river_id, pool_id)
      river = Libertree::Model::River[ account_id: account.id, id: river_id.to_i ]
      pool = Libertree::Model::Pool[ pool_id.to_i ]
      if river && pool
        river.revise(
          'label' => river.label,
          'query' => river.query + %| :spring "#{pool.name}" "#{pool.member.handle}"|
        )
      end
      ''
    end

    def _add_term(river_id, term)
      river = Libertree::Model::River[ account_id: account.id, id: river_id.to_i ]
      if river
        river.revise(
          'label' => river.label,
          'query' => river.query + %| +#{term}|
        )
      end
    end
  end
end
