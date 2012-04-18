require 'uri'
require 'net/http'
require 'socket'
require 'timeout'

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

              profile = Libertree::Model::Profile.find_or_create( member_id: member.id )
              if params['profile']
                profile.name_display = params['profile']['name_display']
                profile.description = params['profile']['description']
              end

              begin
                Timeout.timeout(5) do
                  Net::HTTP.start(uri.host) { |http|
                    resp = http.get(uri.path)
                    ext = File.extname(uri.path)
                    if ! ['.png', '.gif', '.jpg', '.jpeg'].include?(ext.downcase)
                      respond( {
                        'code' => 'ERROR',
                        'message' => "Invalid avatar file type: #{ext}"
                      } )
                      return
                    else
                      File.open( "#{Libertree::Server.conf['avatar_dir']}/#{member.id}#{ext}", 'wb' ) { |file|
                        file.write(resp.body)
                      }
                      member.avatar_path = "/images/avatars/#{member.id}#{ext}"
                    end
                  }
                end
              rescue Timeout::Error
                # ignore
              end

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
