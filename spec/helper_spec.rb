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
      expected = { "parent" =>
        [{ "element" => {"a"=>"1", "b"=>"2", "c"=>"3"}},
         { "element" => {"x"=>"1", "y"=>"2", "z"=>"3"}}
        ]
      }
      expect( helper.xml_to_hash(xml) ).to eq(expected)
    end

    it 'distinguishes between text nodes and nodes with name "text"' do
      s = "<text>hello</text>"

      xml = Blather::Stanza.parse(s)
      expected = { "text" => "hello" }
      expect( helper.xml_to_hash(xml) ).to eq(expected)
    end

    it 'converts references as expected' do
      ref_xml =<<XML
<references>
  <reference>
    <match>(/posts/show/366</match>
    <post>
      <url>/posts/show/366</url>
      <id>366</id>
      <origin>some.remote.tree</origin>
    </post>
  </reference>
  <reference>
    <match> /posts/show/366/128#comment-128</match>
    <post>
      <url>/posts/show/366</url>
      <id>365</id>
      <origin>some.remote.tree</origin>
    </post>
    <comment>
      <url>/128#comment-128</url>
      <id>127</id>
      <origin>some.remote.tree</origin>
    </comment>
  </reference>
  <reference>
    <match>http://never-mind.org/posts/show/366/128</match>
    <post>
      <url>/posts/show/366</url>
      <id>365</id>
      <origin>some.remote.tree</origin>
    </post>
    <comment>
      <url>/128</url>
      <id>127</id>
      <origin>some.remote.tree</origin>
    </comment>
  </reference>
</references>
XML

      expected = { 'references' =>
        [{ 'reference' =>
           { 'match' => '(/posts/show/366',
             'post' => {
               'url' => '/posts/show/366',
               'id' => '366',
               'origin' => 'some.remote.tree' }}
         },
         { 'reference' =>
           { 'match' => ' /posts/show/366/128#comment-128',
             'post' => {
               'url' => '/posts/show/366',
               'id' => '365',
               'origin' => 'some.remote.tree' },
             'comment' => {
               'url' => '/128#comment-128',
               'id' => '127',
               'origin' => 'some.remote.tree' }}
         },
         { 'reference' =>
           { 'match' => 'http://never-mind.org/posts/show/366/128',
             'post' => {
               'url' => '/posts/show/366',
               'id' => '365',
               'origin' => 'some.remote.tree' },
             'comment' => {
               'url' => '/128',
               'id' => '127',
               'origin' => 'some.remote.tree' }}
         }]
      }

      xml = Blather::Stanza.parse(ref_xml)
      helper.xml_to_hash(xml).should eq(expected)
    end

    it 'converts lists as expected' do
      ref_xml =<<XML
<trees>
  <domain>some.remote.tree</domain>
  <domain>some.other.tree</domain>
  <domain>some.same.tree</domain>
</trees>
XML

      expected = { "trees" =>
        [{'domain' => 'some.remote.tree'},
         {'domain' => 'some.other.tree'},
         {'domain' => 'some.same.tree'}]}

      xml = Blather::Stanza.parse(ref_xml)
      helper.xml_to_hash(xml).should eq(expected)
    end

  end
end
