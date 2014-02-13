require 'libertree/server/responder/helper'

module Libertree
  module Server
    module Gateway
      extend Libertree::Server::Responder::Helper

      private
      # XEP-0030: Service Discovery
      def self.disco_info(stanza)
        identity = Blather::Stanza::Iq::DiscoInfo::Identity.
          new({ :name => 'Libertree Gateway',
                :type => 'libertree',
                :category => 'gateway' })

        features = [ "http://jabber.org/protocol/disco#info",
                     "jabber:iq:register" ]

        info = stanza.reply
        info.identities = [identity]
        info.features = features

        @client.write info
      end

      # XEP-0100: Gateway Interaction
      def self.register_query(stanza)
        if stanza.from
          account = Libertree::Model::Account[ gateway_jid: stanza.from.to_s ]
        end

        # if the JID is already registered, respond with record info
        if account
          record_info = Nokogiri::XML::Builder.new { |xml|
            xml.query('xmlns' => 'jabber:iq:register') {
              xml.registered
              xml.username(account.username)
              xml.password(account.password)
            }
          }.doc.root
          respond to: stanza, with: record_info
        else
          prompt = Nokogiri::XML::Builder.new { |xml|
            xml.query('xmlns' => 'jabber:iq:register') {
              xml.instructions("Please provide your Libertree username and password.")
              xml.username
              xml.password
            }
          }.doc.root
          respond to: stanza, with: prompt
        end
      end

      def self.register_auth(stanza)
        ns = 'jabber:iq:register'
        username_node = stanza.xpath('.//ns:username', :ns => ns)
        password_node = stanza.xpath('.//ns:password', :ns => ns)
        unregister = ! stanza.xpath('.//ns:remove', :ns => ns).empty?

        error = Nokogiri::XML.fragment(%{<error code='406' type='modify'>
                                           <not-acceptable
                                             xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
                                         </error>})

        if unregister
          self.register_remove(stanza)
        elsif [username_node, password_node].any?(&:empty?)
          respond to: stanza, with: [error, stanza]
        else
          account = Libertree::Model::Account.authenticate({
            'username' => username_node.text,
            'password' => password_node.text
          })
          if account
            # success!  Try to register jid with account
            begin
              account.gateway_jid = stanza.from.to_s
              @client.write stanza.reply!
              # TODO: presence subscribe
            rescue StandardError => e
              respond to: stanza, with: [error, stanza]
            end
          else
            # failure
            respond to: stanza, with: [error, stanza]
          end
        end
      end

      def self.register_remove(stanza)
        account = Libertree::Model::Account[ gateway_jid: stanza.from.to_s ]
        if account
          account.gateway_jid = nil
        end
        @client.write stanza.reply!

        [ 'unsubscribe', 'unsubscribed', 'unavailable' ].each do |presence_type|
          p = Blather::Stanza::Presence.new
          p.to = stanza.to
          p.from = stanza.from
          p.type = presence_type
          @client.write p
        end
      end

      public
      def self.init(client)
        # set @client for the `respond` helper method
        @client = client

        client.register_handler(:disco_info) {|stanza| disco_info(stanza)}
        client.register_handler :iq, '/iq/ns:query', :ns => 'jabber:iq:register' do |stanza|
          if stanza.get?
            register_query(stanza)
          elsif stanza.set?
            register_auth(stanza)
          end
        end
      end
    end
  end
end
