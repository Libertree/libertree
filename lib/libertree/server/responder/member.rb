require 'uri'
require 'net/http'
require 'socket'

module Libertree
  module Server
    module Responder
      module Member
        def rsp_member(params)
          return  if require_parameters(params, 'username', 'avatar_url')

          begin
            uri = URI.parse(params['avatar_url'])
            ip = Socket.getaddrinfo(uri.host, 'http')[0][3]

            if ip != @server.ip
              respond( {
                'code' => 'ERROR',
                'message' => "avatar_url does not resolve to IP #{@server.ip}"
              } )
            else
              member = Model::Member.find_or_create(
                'username' => params['username'],
                'server_id' => @server.id
              )
              Net::HTTP.start(uri.host) { |http|
                resp = http.get(uri.path)
                File.open( "#{Libertree::Server.conf['avatar_dir']}/#{member.id}.png", 'wb' ) { |file|
                  file.write(resp.body)
                }
              }
              member.avatar_path = "/images/avatars/#{member.id}.png"

              respond_with_code 'OK'
            end
          rescue URI::InvalidURIError => e
            respond( {
              'code' => 'ERROR',
              'message' => "Invalid URI: #{params['avatar_url']}"
            } )
          rescue PGError => e
            respond_with_code 'ERROR'
          end
        end
      end
    end
  end
end
