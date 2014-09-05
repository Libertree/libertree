require 'libertree/server/responder/helper'

module Libertree
  module Server
    module Gateway
      extend Libertree::Server::Responder::Helper

      private
      def self.init_disco_info
        Libertree::Server::Disco.register_identity({ :name => 'Libertree Gateway',
                                                     :type => 'libertree',
                                                     :category => 'gateway' })
        Libertree::Server::Disco.register_feature "jabber:iq:register"
      end

      # XEP-0100: Gateway Interaction
      def self.register_query(stanza)
        if stanza.from
          account = Libertree::Model::Account[ gateway_jid: stanza.from.stripped.to_s ]
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
                                           <not-acceptable xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
                                         </error>})

        if unregister
          self.register_remove(stanza)
        elsif [username_node, password_node].any?(&:empty?)
          respond to: stanza, with: [error, stanza.children.first], type: :error
        else
          account = Libertree::Model::Account.authenticate({
            'username' => username_node.text,
            'password' => password_node.text
          })
          if account
            # success!  Try to register jid with account
            begin
              account.gateway_jid = stanza.from.stripped.to_s
              account.save
              # distribute member record with gateway_jid
              account.member.gateway_jid = account.gateway_jid
              account.member.save
              @client.write stanza.reply!

              # subscribe to user presence
              @client.write Blather::Stanza::Presence::Subscription.
                new(stanza.from.stripped, :subscribe)
            rescue StandardError => e
              respond to: stanza, with: [error, stanza.children.first], type: :error
            end
          else
            # failure
            respond to: stanza, with: [error, stanza.children.first], type: :error
          end
        end
      end

      def self.register_remove(stanza)
        with_account(stanza) do |account|
          account.gateway_jid = nil
          account.save
          # distribute member record with gateway_jid
          account.member.gateway_jid = nil
          account.member.save
        end
        @client.write stanza.reply!

        [ 'unsubscribe', 'unsubscribed', 'unavailable' ].each do |type|
          p = Blather::Stanza::Presence::Subscription.new(stanza.to, type)
          p.from = stanza.from.stripped
          @client.write p
        end
      end

      def self.log_out(stanza)
        with_account(stanza) do |account|
          # TODO: forward "unavailable" presence to all Libertree contacts
          p = Blather::Stanza::Presence::Subscription.new(stanza.from.stripped, :unavailable)
          @client.write p
        end
      end

      public
      def self.init(client)
        # set @client for the `respond` helper method
        @client = client

        init_disco_info
        client.register_handler :iq, '/iq/ns:query', :ns => 'jabber:iq:register' do |stanza|
          if stanza.get?
            register_query(stanza)
          elsif stanza.set?
            register_auth(stanza)
          end
        end

        # only approve subscription requests from users that are
        # already registered with the gateway
        client.register_handler :subscription, :request? do |subscription|
          account = Libertree::Model::Account[ gateway_jid: subscription.from.stripped.to_s ]
          if account
            # TODO: register subscription somewhere...
            @client.write subscription.approve!
          else
            @client.write subscription.refuse!
          end
        end

        client.register_handler :presence, :unavailable? do |stanza|
          log_out stanza
        end

        client.register_handler :presence do |stanza|
          with_account(stanza) do
            @client.write stanza.reply!
            # TODO: If this is directed presence, report presence of
            # Libertree user, if subscribed to that user.
          end
        end

      end
    end
  end
end
