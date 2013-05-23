require 'uri'
require 'socket'
require 'timeout'

module Libertree
  module Server
    module Responder
      module Member
        def rsp_member(params)
          require_parameters(params, 'username')

          begin
            member = Model::Member.find_or_create(
              'username' => params['username'],
              'server_id' => @remote_tree.id
            )

            profile = Libertree::Model::Profile.find_or_create( member_id: member.id )

            if params['profile']
              begin
                profile.name_display = params['profile']['name_display']
              rescue PGError => e
                if e.message =~ /valid_name_display/
                  fail InternalError, "Invalid display name: #{params['profile']['name_display'].inspect}", nil
                else
                  raise e
                end
              end
              profile.description = params['profile']['description']
            end

            # fetch avatar asynchronously
            if params['avatar_url']
              URI.parse(params['avatar_url'])

              Libertree::Model::Job.create(
                task: 'http:avatar',
                params: {
                  'member_id'  => member.id,
                  'avatar_url' => params['avatar_url'],
                }.to_json
              )
            end
          rescue URI::InvalidURIError => e
            fail InternalError, "Invalid URI: #{params['avatar_url']}", nil
          rescue PGError => e
            log "Error in rsp_member: #{e.message}"
            fail InternalError, '', nil
          end
        end

        def rsp_member_delete(params)
          require_parameters(params, 'username')

          begin
            members = Model::Member.
              where( 'username' => params['username'] ).
              reject { |p| p.server != @remote_tree }

            assert members[0], "Unrecognized username: #{params['username'].inspect}"
            members[0].delete_cascade  # there should only be one member
          rescue PGError => e
            log "Error in rsp_member_delete: #{e.message}"
            fail InternalError, '', nil
          end
        end
      end
    end
  end
end
