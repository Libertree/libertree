module Libertree
  module Server
    module Responder
      module Authentication
        def rsp_introduce(params)
          public_key = params['public_key']

          server = Model::Server[ 'public_key' => public_key ]
          if server.nil?
            Model::Server.create(
              'ip'         => @ip_remote,
              'public_key' => public_key
            )
            respond_with_code 'OK'
          else
            challenge_encrypted = nil

            $gpg_mutex ||= Mutex.new
            $gpg_mutex.synchronize do
              GPGME::Key.import(public_key)
              crypto = GPGME::Crypto.new( armor: true )
              challenge = (0...32).map { ( 32 + rand(94) ).chr }.join
              challenge_encrypted = crypto.encrypt(challenge, armor: true, always_trust: true).read
            end

            respond( {
              'code' => 'OK',
              'challenge' => challenge_encrypted,
            } )
          end
        end
      end
    end
  end
end

