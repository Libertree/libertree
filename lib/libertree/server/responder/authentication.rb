module Libertree
  module Server
    module Responder
      module Authentication
        def rsp_introduce(params)
          public_key = params['public_key']
          server = Model::Server[ 'public_key' => public_key ]
          if server
            # TODO: Encrypt for challenge
            respond( {
              'code' => 'OK',
              'challenge' => '1234567890abcdef',
            } )
          else
            Model::Server.create(
              'ip'         => @ip_remote,
              'public_key' => public_key
            )
            respond_with_code 'OK'
          end
        end
      end
    end
  end
end

