module Libertree
  module Server
    module Responder
      module Authentication
        def rsp_authenticate(params)
          if params['response'] != @server.challenge
            respond 'code' => 'ERROR', 'message' => 'Challenge failed.'
            close_connection_after_writing
          else
            @server.authenticated = true
            respond_with_code 'OK'
          end
        end

        def rsp_introduce(params)
          public_key = params['public_key']

          @server = Model::Server[ 'public_key' => public_key ]
          if @server.nil?
            @server = Model::Server.create(
              'ip'         => @ip_remote,
              'public_key' => public_key,
            )
            @server.extend Authenticatable
            @server.authenticated = true
            respond_with_code 'OK'
          else
            @server.extend Authenticatable

            challenge = challenge_new
            challenge_encrypted = nil

            $gpg_mutex ||= Mutex.new
            $gpg_mutex.synchronize do
              GPGME::Key.import(public_key)
              crypto = GPGME::Crypto.new( armor: true )
              challenge_encrypted = crypto.encrypt(challenge, armor: true, always_trust: true).read
            end

            @server.challenge = challenge

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
