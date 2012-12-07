require 'libertree/server/responder/dispatcher'

require 'libertree/server/responder/chat'
require 'libertree/server/responder/comment'
require 'libertree/server/responder/comment-like'
require 'libertree/server/responder/forest'
require 'libertree/server/responder/member'
require 'libertree/server/responder/message'
require 'libertree/server/responder/pool'
require 'libertree/server/responder/pool-post'
require 'libertree/server/responder/post'
require 'libertree/server/responder/post-like'

module Libertree
  module Server
    module Responder
      include Dispatcher

      include Chat
      include Comment
      include CommentLike
      include Forest
      include Member
      include Message
      include Pool
      include PoolPost
      include Post
      include PostLike

      # @param [Hash] params A Hash.
      # @param [Array] required_parameters The keys which are required.
      # @return [Array] The keys whose values are missing.
      def missing_parameters(params, *required_parameters)
        missing = []
        required_parameters.each do |rp|
          if params[rp].nil? || params[rp].respond_to?(:empty?) && params[rp].empty?
            missing << rp
          end
        end
        missing
      end

      # Calls #missing_parameters.  If any parameters are missing, raises
      # MissingParameters exception with the first missing parameter as message.
      # @return [nil] when there are no missing parameters
      # @raises [MissingParameter] when there is a missing parameter
      def require_parameters(*args)
        mp = missing_parameters(*args)
        if mp[0]
          fail MissingParameter, mp[0], nil
        end
      end

      def assert(obj, msg)
        if obj.nil?
          fail NotFound, msg, nil
        end
      end
    end
  end
end
