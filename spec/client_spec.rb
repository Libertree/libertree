require 'spec_helper'
require 'openssl'
require_relative '../lib/libertree/client'

describe Libertree::Client do
  before(:each) do
    key = OpenSSL::PKey::RSA.new File.read("private.key")
    @c = Libertree::Client.new({ private_key: key,
                                 public_key: key.public_key.to_pem,
                                 avatar_url_base: "localhost" })
  end

  describe 'build_stanza' do
    it 'constructs a valid iq stanza' do
      stanza = @c.send(:build_stanza,
        'libertree.localhost.localdomain',
        { 'post' => { 'text' => 'Hello',
                      'id'   => 123 }}
      )
      example =<<HERE
<iq type="set" to="libertree.localhost.localdomain" id="blather0001">
  <libertree xmlns="libertree">
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
      xml = @c.send(:params_to_xml,
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

    it 'encodes entities' do
      xml = @c.send(:params_to_xml,
                    { 'something' =>
                      "This is a text --> hello & goodbye. A <link>" })
      example = "<something>This is a text --&gt; hello &amp; goodbye. A &lt;link&gt;</something>"
      xml.should == example
    end
  end
end
