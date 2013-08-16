require 'blather'
require 'spec_helper'
require 'libertree/client'

describe Libertree::Server::Responder::Helper do
  let(:helper_class) { Class.new }
  let(:helper) { helper_class.new }

  before :each do
    helper_class.class_eval {
      include Libertree::Server::Responder::Helper
    }
  end

  describe 'xml_to_hash' do
    it 'converts elements at the same level to an array' do
      s = "<parent><element><a>1</a><b>2</b><c>3</c></element><element><x>1</x><y>2</y><z>3</z></element></parent>"

      xml = Blather::Stanza.parse(s)
      helper.xml_to_hash(xml).should eq({"parent"=>{"element"=>[{"a"=>"1", "b"=>"2", "c"=>"3"}, {"x"=>"1", "y"=>"2", "z"=>"3"}]}})
    end
  end
end
