module Controller
  module API
    module V1
      class Posts < Base
        map '/api/v1/posts'

        layout nil

        before_all do
          set_account_from_token
        end

        def create
          if ! request.post?
            respond '', 405
          end

          if request['source'].nil? || request['source'].to_s.strip.empty? ||
              Libertree.plain( request['source'].to_s ).empty?
            respond '', 400
          end

          visibility = request['visibility'] || 'forest'
          visibility = visibility.to_s

          text = request['text'].to_s
          if Libertree::Model::Post.urls_already_posted?(text)
            respond '', 403
          else
            post = Libertree::Model::Post.create(
              member_id:  @account.member.id,
              visibility: visibility,
              text:       text,
              via:        Libertree.plain( request['source'].to_s )
            )

            { 'success' => true, 'id' => post.id }.to_json
          end
        end
      end
    end
  end
end
