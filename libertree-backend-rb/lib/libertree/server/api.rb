require 'libertree/server/responder/helper'

module Libertree
  module Server
    module Api
      extend Libertree::Server::Responder::Helper

      private
      def self.post(account, msg)
        Libertree::Model::Post.create(
          member_id:  account.member.id,
          visibility: 'forest',
          text:       msg,
          via:        'xmpp'
        )
      end

      def self.reply(account, id, msg)
        post = Libertree::Model::Post[ id.to_i ]
        if post
          # TODO: create a comment on this post only if account has permissions
          Libertree::Model::Comment.create(
            member_id:  account.member.id,
            post_id:    post.id,
            text:       msg
          )
        end
      end

      def self.try_api(account, stanza)
        body = stanza.body
        body.match(%r{^POST (?<msg>.*)}) do |m|
          post(account, m[:msg])
          throw :halt
        end
        body.match(%r{^REPLY (?<id>\d+) (?<msg>.*)}) do |m|
          reply(account, m[:id], m[:msg])
          throw :halt
        end
      end

      public
      def self.init(client)
        @client = client
        client.register_handler :message, :chat?, :body do |stanza|
          with_account(stanza) {|a,s| try_api(a,s)}
        end
      end
    end
  end
end
