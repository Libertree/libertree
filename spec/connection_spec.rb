require 'spec_helper'
require_relative '../lib/libertree/connection'

describe Libertree::Connection do
  before(:each) do
    @c = Libertree::Connection.new({})
  end

  describe 'build_stanza' do
    it 'constructs a valid iq stanza' do
      stanza = @c.build_stanza(
        'libertree.localhost.localdomain',
        'POST',
        {
          'text' => 'Hello',
          'id'   => 123
        }
      )
      example =<<HERE
<iq type="get" to="libertree.localhost.localdomain" id="blather0001">
  <libertree>
    <post>
      <text>Hello</text>
      <id>123</id>
    </post>
  </libertree>
</iq>
HERE
      stanza.to_xml.should == Nokogiri::XML::fragment(example).to_xml.strip
    end
  end

  describe 'params_to_xml' do
    it 'constructs a valid xml fragment' do
      xml = @c.params_to_xml(
        {
          'id' => 12,
          'trees' =>
          [
            'lt1.remote.org',
            'lt2.remote.org',
            'lt3.remote.org',
          ].map {|t| { 'domain' => t }},
          'name' => 'nothing'
        }
      )
      example = <<HERE
<id>12</id>
<trees>
  <domain>lt1.remote.org</domain>
  <domain>lt2.remote.org</domain>
  <domain>lt3.remote.org</domain>
</trees>
<name>nothing</name>
HERE
      xml.should == example.delete("\n ")
    end
  end
end
