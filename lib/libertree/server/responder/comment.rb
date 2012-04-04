module Libertree
  module Server
    module Responder
      module Comment
        def rsp_comment(params)
          return  if require_parameters(params, 'username', 'post_id', 'text')

          begin
            member = Model::Member[
              'username' => params['username'],
              'server_id' => @server.id,
            ]

            if member.nil?
              respond( {
                'code' => 'NOT FOUND',
                'message' => "Unrecognized member username: #{params['username'].inspect}"
              } )
            else

              post = Model::Post[ remote_id: params['post_id'], member_id: member.id ]
              if post.nil?
                respond( {
                  'code' => 'NOT FOUND',
                  'message' => "Unrecognized post ID: #{params['post_id'].inspect}"
                } )
              else
                Model::Comment.find_or_create(
                  'member_id' => member.id,
                  'post_id' => post.id,
                  'text' => params['text']
                )
                respond_with_code 'OK'
              end
            end
          rescue PGError => e
            respond_with_code 'ERROR'
          end
        end
      end
    end
  end
end
