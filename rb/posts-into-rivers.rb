require 'libertree/model'

include Libertree::Model

Post.each do |p|
  p.add_to_matching_rivers
  $stdout.print '.'; $stdout.flush
end
