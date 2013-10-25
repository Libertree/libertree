require 'spec_helper'
require 'openssl'
require_relative '../lib/libertree/client'

describe Libertree::Client do
  before(:each) do
    key = OpenSSL::PKey::RSA.new File.read("private.key")
    @c = Libertree::Client.new({ private_key: key,
                                 contact: 'admin@localhost',
                                 domain: 'localhost',
                                 frontend_url_base: 'localhost' })
  end

  describe 'build_stanza' do
    it 'constructs a valid iq stanza' do
      stanza = @c.send(:build_stanza,
        'libertree.localhost.localdomain',
        { 'post' => { 'text' => 'Hello',
                      'id'   => 123 }}
      )
      example = %{
<iq type="set" to="libertree.localhost.localdomain" id="blather0001">
  <libertree xmlns="urn:libertree">
    <post>
      <text>Hello</text>
      <id>123</id>
    </post>
  </libertree>
</iq>
}
      expect(stanza.to_xml).to eq Nokogiri::XML::fragment(example).to_xml.strip
    end
  end

  describe 'params_to_xml' do
    it 'constructs a valid xml fragment' do
      params = {
        'id' => 12,
        'trees' => [ 'lt1.remote.org',
                     'lt2.remote.org',
                     'lt3.remote.org',
                   ].map {|t| { 'domain' => t }},
        'name' => 'nothing',
        'array' => [ 1, 2, 3,
                     { 'inner' => [4,5,"<test>",[6,7]] },
                     4, 5 ],
      }
      xml = @c.send(:params_to_xml, params)
      example = %{
<id>12</id>
<trees>
  <domain>lt1.remote.org</domain>
  <domain>lt2.remote.org</domain>
  <domain>lt3.remote.org</domain>
</trees>
<name>nothing</name>
<array>
  <element>1</element>
  <element>2</element>
  <element>3</element>
  <inner>
    <element>4</element>
    <element>5</element>
    <element>&lt;test&gt;</element>
    <element>
      <element>6</element>
      <element>7</element>
    </element>
  </inner>
  <element>4</element>
  <element>5</element>
</array>
}
      expect(xml).to eq example.delete("\n ")
    end

    it 'encodes entities' do
      xml = @c.send(:params_to_xml,
                    { 'something' =>
                      "This is a text --> hello & goodbye. A <link>" })
      example = "<something>This is a text --&gt; hello &amp; goodbye. A &lt;link&gt;</something>"
      expect(xml).to eq example
    end
  end
end
