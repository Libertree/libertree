module Libertree
  module Server
    module Responder
      module Authentication
        def rsp_authenticate(params)
          if params['response'] != @server.challenge
            respond 'code' => 'ERROR', 'message' => 'Challenge failed.'
            close_connection_after_writing
          else
            log "Server #{@server.id} authenticated."
            @server.authenticated = true
            if params['name'] && ! params['name'].strip.empty?
              @server.name_given = params['name'].strip
            end
            if params['domain'] && ! params['domain'].strip.empty?
              @server.domain = params['domain'].strip
            end
            if params['contact'] && ! params['contact'].strip.empty?
              @server.contact = params['contact'].strip
            end
            @server.ip = @ip_remote
            respond_with_code 'OK'
          end
        end

        def rsp_introduce(params)
          if @ip_remote == Server.conf['ip_public'] || @ip_remote == Server.conf['ip_listen']
            respond_with_code 'ERROR'
            return
          end

          public_key = params['public_key']
          if public_key.nil? || public_key.strip.empty?
            respond_with_code 'MISSING PARAMETER'
            return
          end

          @server = Model::Server[ 'public_key' => public_key ]
          if @server.nil?
            @server = Model::Server.create(
              'ip'         => @ip_remote,
              'public_key' => public_key,
            )
            @server.extend Authenticatable
            @server.authenticated = true

            log "#{@ip_remote} is a new server (id: #{@server.id})."

            respond_with_code 'OK'
          else
            @server.extend Authenticatable
            @server.challenge = challenge_new
            key = OpenSSL::PKey::RSA.new public_key
            challenge_encrypted = Base64.encode64(key.public_encrypt(@server.challenge, OpenSSL::PKey::RSA::PKCS1_OAEP_PADDING))

            log "#{@ip_remote} provided public key of #{@server.name_given || 'server ' + @server.id.to_s} ."

            respond( {
              'code' => 'OK',
              'challenge' => challenge_encrypted,
            } )
          end
        end

        def challenge_new
          (0...32).map { ( 32 + rand(94) ).chr }.join
        end
      end
    end
  end
end
