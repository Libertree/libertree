require 'libertree/server/responder/helper'

module Libertree
  module Server
    module PubSub
      extend Libertree::Server::Responder::Helper
      Disco = Libertree::Server::Disco

      # There are three first-level nodes:
      #   /users     --- contains a collection node for each user
      #   /posts     --- a leaf node listing all (public) posts on this server
      #   /groups    --- a collection node of all groups on this server
      #
      # The /users collection contains a collection node for each
      # user which contains three collection nodes:
      #   /posts     --- a leaf node containing all posts by this user
      #   /springs   --- a collection of all springs by this user
      #
      # All lower-level nodes need to be addressed with a complete
      # path, e.g.:
      #   /users/rekado/posts
      #
      # The following is an example of a node hierarchy:
      #   /users
      #     /<username>
      #       /posts
      #         /<post id>
      #         ...
      #       /springs
      #         /<spring id>
      #           /<post id>
      #           /<post id>
      #           ...
      #         /<spring id>
      #         ...
      #     /<username>
      #       /posts
      #       /springs
      #     ...
      #   /posts
      #     /<post id>
      #     /<post id>
      #     ...
      #   /groups
      #     /<group id>
      #     ...

      private
      @features =
        [ 'http://jabber.org/protocol/disco#items',
          'http://jabber.org/protocol/pubsub',
          'http://jabber.org/protocol/pubsub#retrieve-subscriptions',
          'http://jabber.org/protocol/pubsub#retrieve-affiliations' ]

      def self.check_pubsub_feature(node, stanza, feature)
        # fail if node doesn't exist
        # or if node exists but has no such feature
        unless node && self.node_identities_features(node.address).last.
            include?("http://jabber.org/protocol/pubsub##{feature}")
          unsup = Nokogiri::XML::Builder.new {|x|
            x.send('unsupported', {
                     xmlns: 'http://jabber.org/protocol/pubsub#errors',
                     feature: feature
                   })
          }.doc.root
          Blather::StanzaError.new(stanza, 'feature-not-implemented', 'cancel', nil, [unsup])
        end
      end

      def self.node_identities_features(path)
        return [{}, []] unless path

        res = path.match %r{^/users/(?<username>[^/]+)(/springs)?$}
        if ['/users', '/groups'].include?(path) ||
            (res && Libertree::Model::Account[ username: res[:username] ])
          return [{ :type => 'collection',
                    :category => 'pubsub'
                  }, @features ]
        end

        res = path.match %r{^/users/(?<username>[^/]+)/(posts|springs/\d+)$}
        if path == '/posts' ||
            (res && Libertree::Model::Account[ username: res[:username] ])
          return [{ :type => 'leaf',
                    :category => 'pubsub'
                  }, @features ]
        end

        # no identities, no features
        return [{}, []]
      end

      def self.init_disco_info
        # register identity and features of root node
        Disco.register_identity({ :name => 'Libertree PubSub',
                                  :type => 'service',
                                  :category => 'pubsub' })
        @features.each {|f| Disco.register_feature f }

        Disco.register_dynamic_node_info method(:node_identities_features)
       end

      def self.user_path(username, &blk)
        account = Libertree::Model::Account[ username: username ]
        yield(account)  if account
      end

      def self.items(node_path)
        # TODO: use result set management instead of only return the
        # default set size of 30 posts:
        #
        #  <set xmlns='http://jabber.org/protocol/rsm'>
        #    <first index='0'>/posts/123</first>
        #    <last>/posts/140</last>
        #    <count>100234</count>
        #  </set>

        # TODO: limit to internet visible posts only?  At the moment
        # this is not a problem, because we don't support
        # subscriptions yet

        case node_path
        when nil
          # return top level nodes
          items = [ ['/users',  'User nodes'],
                    ['/posts',  'Public posts'],
                    ['/groups', 'Public groups']]
        when '/users'
          items = Libertree::Model::Account.all.map {|a| [ "/users/#{a.username}", a.member.name_display ]}
        when %r{^/users/([^/]+)$}
          items = user_path($1) do |a|
            [ ["/users/#{a.username}/posts",   'Public posts'],
              ["/users/#{a.username}/springs", 'Springs'],
            ]
          end
        when %r{^/users/([^/]+)/posts$}
          items = user_path($1) do |a|
            a.member.posts.map {|post| [ nil, "/users/#{a.username}/posts/#{post.id}" ]}
          end
        when %r{^/users/([^/]+)/springs$}
          items = user_path($1) do |a|
            a.member.springs.map {|spring| [ "/users/#{a.username}/springs/#{spring.id}", spring.name ]}
          end
        when %r{^/users/([^/]+)/springs/(\d+)$}
          items = user_path($1) do |a|
            spring = Libertree::Model::Pool[ member_id: a.member.id, sprung: true, id: $2.to_i ]
            return  unless spring
            spring.posts.map {|post| [ nil, "/users/#{a.username}/springs/#{spring.id}/#{post.id}" ]}
          end
        when '/posts'
          items = Libertree::Model::Post.limit(10).
            map {|post| [ nil, "/posts/#{post.id}" ]}
        when '/groups'
          items = []  # TODO: groups are not implemented yet
        else return end

        items = items.map do |item|
          Blather::Stanza::DiscoItems::Item.
            new(@jid, *item)
        end

        Blather::Stanza::DiscoItems.new(:result, node_path, items).children
      end

      def self.retrieve_subscriptions(stanza)
        subs = []

        # TODO: upstream bug: stanza.subscriptions duplicates the node
        # node_name = stanza.subscriptions.attr('node')
        ns = Blather::Stanza::PubSub::Subscriptions.new.class.registered_ns
        node_name  = stanza.pubsub.find_first('ns:subscriptions', ns: ns).attr('node')

        if node_name
          node = Libertree::Model::Node[ address: node_name, server_id: nil ]
          if err = self.check_pubsub_feature(node, stanza, 'retrieve-subscriptions')
            @client.write err.to_node
            return
          end

          subs = node.subs(stanza.from.stripped)
        else
          # return all subs if no node is requested
          subs = Libertree::Model::NodeSubscription.for(stanza.from.stripped)
        end

        response = Blather::Stanza::PubSub::Subscriptions.new(:result)
        # TODO: upstream bug: response.subscriptions duplicates the node
        # parent = response.subscriptions
        parent = response.pubsub.find_first('ns:subscriptions', ns: response.class.registered_ns)

        subs.each do |sub|
          child = Blather::Stanza::PubSub::Subscription.
            new(:result, @jid, sub.node.address, sub.jid, sub.sub_id, sub.state.to_sym).
            subscription_node
          parent.add_child(child)
          child.namespace = response.class.registered_ns
        end
        respond to: stanza, with: response.pubsub
      end

      def self.retrieve_affiliations(stanza)
        affs = []
        node_name = stanza.affiliations.attr('node')

        if node_name
          node = Libertree::Model::Node[ address: node_name, server_id: nil ]
          if err = self.check_pubsub_feature(node, stanza, 'retrieve-affiliations')
            @client.write err.to_node
            return
          end

          affs = node.affiliations.where(jid: stanza.from.stripped.to_s)
        else
          # return all affiliations if no node is requested
          affs = Libertree::Model::NodeAffiliation.where(jid: stanza.from.stripped.to_s)
        end

        response = Blather::Stanza::PubSub::Affiliations.new(:result)
        affs.each do |aff|
          child = Nokogiri::XML::Builder.new {|x|
            x.send('affiliation', {node: aff.node.address, affiliation: aff.affiliation})
          }.doc.root
          response.affiliations << child
        end

        respond to: stanza, with: response.pubsub
      end

      public
      def self.init(client, jid)
        init_disco_info

        @client = client
        @jid = jid

        client.register_handler :disco_items do |stanza|
          if response = self.items(stanza.node)
            respond to: stanza, with: response
          else
            @client.write Blather::StanzaError.new(stanza, 'service-unavailable', 'cancel').to_node
          end
        end

        client.register_handler :pubsub_subscriptions do |stanza|
          self.retrieve_subscriptions(stanza)
        end

        client.register_handler :pubsub_affiliations do |stanza|
          self.retrieve_affiliations(stanza)
        end
     end
    end
  end
end
