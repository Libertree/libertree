require 'libertree/server/responder/dispatcher'
require 'libertree/server/responder/authentication'

module Libertree
  module Server
    module Responder
      include Dispatcher
      include Authentication
    end
  end
end
