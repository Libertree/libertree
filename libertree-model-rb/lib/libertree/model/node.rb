module Libertree
  module Model
    class Node < Sequel::Model(:nodes)
      ACCESS_MODELS = [ :open,
                        :presence,
                        :roster,
                        :authorize,
                        :whitelist ]
      def affiliations
        NodeAffiliation.where(:node_id => self.id)
      end

      def subs(jid=nil)
        NodeSubscription.for(jid).where(:node_id => self.id)
      end

      def local_subscribers
        self.subs.join(:accounts, :id => :account_id)
      end
    end
  end
end
