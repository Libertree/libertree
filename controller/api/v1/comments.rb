module Controller
  module API
    module V1
      class Comments < Base
        map '/api/v1/comments'

        layout nil

        before_all do
          set_account_from_token
        end

        def create
          if ! request.post?
            respond '', 405
          end

          if request['post_id'].nil? || request['post_id'].to_i.zero?
            respond '', 400
          end

          post = Libertree::Model::Post[ request['post_id'].to_i ]
          if post.nil?
            respond '', 400
          end

          comment = Libertree::Model::Comment.create(
            member_id:  @account.member.id,
            post_id:    post.id,
            text:       request['text'].to_s
          )

          { 'success' => true, 'id' => comment.id }.to_json
        end
      end
    end
  end
end
