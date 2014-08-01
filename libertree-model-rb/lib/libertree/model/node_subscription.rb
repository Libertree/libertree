module Libertree
  module Model
    class NodeSubscription < Sequel::Model(:node_subscriptions)
      STATES = [ :none,
                 :pending,
                 :unconfigured,
                 :subscribed ]

      many_to_one :node

      def self.for(jid_or_host)
        return self  unless jid_or_host
        jid_or_host = jid_or_host.to_s
        if jid_or_host.include?('@')
          self.where(jid: jid_or_host)
        else
          host_pattern = self.where.escape_like(jid_or_host.to_s)
          self.where(Sequel.like(:jid, "%@#{host_pattern}"))
        end
      end
    end
  end
end
