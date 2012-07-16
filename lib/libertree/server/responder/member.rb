require 'uri'
require 'socket'
require 'timeout'

module Libertree
  module Server
    module Responder
      module Member
        def rsp_member(params)
          return  if require_parameters(params, 'username', 'avatar_url')

          begin
            # verify uri
            URI.parse(params['avatar_url'])

            member = Model::Member.find_or_create(
              'username' => params['username'],
              'server_id' => @server.id
            )

            # fetch avatar asynchronously
            Libertree::Model::Job.create(
              task: 'http:avatar',
              params: {
                'member_id'  => member.id,
                'avatar_url' => params['avatar_url'],
              }.to_json
            )

            profile = Libertree::Model::Profile.find_or_create( member_id: member.id )

            if params['profile']
              begin
                profile.name_display = params['profile']['name_display']
              rescue PGError => e
                if e.message =~ /valid_name_display/
                  respond( {
                    'code' => 'ERROR',
                    'message' => "Invalid display name: #{params['profile']['name_display'].inspect}"
                  } )
                  return
                else
                  raise e
                end
              end
              profile.description = params['profile']['description']
            end

            respond_with_code 'OK'
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
