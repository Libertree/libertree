module Libertree
  module Model
    class NodeAffiliation < Sequel::Model(:affiliations)
      TYPES = [ :owner,
                :publisher,
                :'publish-only',
                :member,
                :none,
                :outcast ]

      many_to_one :node
    end
  end
end
