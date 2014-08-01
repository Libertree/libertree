require 'libertree/server/responder/helper'

module Libertree
  module Server
    module Disco
      # XEP-0030: Service Discovery
      extend Libertree::Server::Responder::Helper

      private
      def self.disco_info(stanza)
        info = stanza.reply
        info.node = stanza.node

        _identities = @@identities[stanza.node]
        _features = @@features[stanza.node]

        # evaluate rules to get dynamic features and identities
        @@rules.each do |rule|
          identity, features = rule.call(stanza.node)
          if (identity && ! identity.empty?)
            _identities << Blather::Stanza::Iq::DiscoInfo::Identity.new(identity)
          end
          if (features && ! features.empty?)
            _features << features
          end
        end

        info.identities = _identities
        info.features = _features
        @client.write info
      end

      public
      def self.register_feature(feature, node=nil)
        return  unless feature
        @@features[node] << feature
      end

      def self.register_identity(identity, node=nil)
        return  unless identity
        @@identities[node] << identity
      end

      def self.register_dynamic_node_info(proc)
        @@rules << proc
      end

      def self.init(client)
        # set @client for the `respond` helper method
        @client = client

        # rules for dynamic node-based identities / features
        @@rules = []

        # return a new empty array for each unknown key
        @@identities = {}
        @@identities.default_proc = proc {|h,k| h[k] = []}

        @@features = {}
        @@features.default_proc = proc {|h,k| h[k] = []}

        self.register_feature "http://jabber.org/protocol/disco#info"
        client.register_handler(:disco_info) {|stanza| disco_info(stanza)}
      end
    end
  end
end
