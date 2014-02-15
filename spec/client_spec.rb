require 'spec_helper'
require 'openssl'
require_relative '../lib/libertree/client'

describe Libertree::Client do
  before :each do
    Libertree::Client.any_instance.stub(:connect)
    key = OpenSSL::PKey::RSA.new File.read("private.key")
    @public_key = key.public_key.to_pem
    @contact = "admin@localhost"
    @domain = "localhost"
    @server_name = "server"

    @c = Libertree::Client.new({ private_key: key,
                                 contact: @contact,
                                 domain: @domain,
                                 frontend_url_base: 'localhost' })

    @c.instance_variable_set(:@contact, @contact)
    @c.instance_variable_set(:@domain, @domain)
    @c.instance_variable_set(:@server_name, @server_name)
  end

  describe 'build_stanza' do
    it 'constructs a valid iq stanza' do
      stanza = @c.send(:build_stanza,
        'libertree.localhost.localdomain',
        'content'
      )
      example = %{
<iq type="set" to="libertree.localhost.localdomain" id="blather0001">
  <libertree xmlns="urn:libertree">content</libertree>
</iq>
}
      expect(stanza.to_xml).to eq Nokogiri::XML::fragment(example).to_xml.strip
    end
  end

  describe 'build_references_xml' do
    it 'converts a reference array to xml' do
      expected =<<XML
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

      refs = [{ "match" => "(/posts/show/366",
                "post" => {
                  "url" => "/posts/show/366",
                  "id" => 366,
                  "origin" => "some.remote.tree" }},
              { "match" => " /posts/show/366/128#comment-128",
                "post" => {
                  "url" => "/posts/show/366",
                  "id" => 365,
                  "origin" => "some.remote.tree" },
                "comment" => {
                  "url" => "/128#comment-128",
                  "id" => 127,
                  "origin" => "some.remote.tree" }},
              { "match" => "http://never-mind.org/posts/show/366/128",
                "post" => {
                  "url" => "/posts/show/366",
                  "id" => 365,
                  "origin" => "some.remote.tree" },
                "comment" => {
                  "url" => "/128",
                  "id" => 127,
                  "origin" => "some.remote.tree" }}]
      expect( @c.send(:build_references_xml, refs).to_xml ).to eq(expected.strip)
    end
  end

  context 'with a few test entities' do
    before :all do
      @forest = Libertree::Model::Forest.create( FactoryGirl.attributes_for(:forest) )
      @requester = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
      @forest.add @requester

      @member = Libertree::Model::Member.create(
        FactoryGirl.attributes_for(:member, :server_id => @requester.id)
      )
      @member.profile.description = "foo bar"
      @member.profile.name_display = "Jim"

      @member2 = Libertree::Model::Member.create(
        FactoryGirl.attributes_for(:member, :server_id => @requester.id)
      )

      local_account = Libertree::Model::Account.create(
        FactoryGirl.attributes_for(:account)
      )
      @member3 = local_account.member

      @message = Libertree::Model::Message.create_with_recipients(:text => "hello world", :sender_member_id => @member3.id, :recipient_member_ids => [@member.id, @member2.id])

      @post = Libertree::Model::Post.create(
        FactoryGirl.attributes_for(:post, member_id: @member.id)
      )
      @post_like = Libertree::Model::PostLike.create(
        FactoryGirl.attributes_for(:post_like, post_id: @post.id, member_id: @member.id)
      )
      @comment = Libertree::Model::Comment.create(
        FactoryGirl.attributes_for(:comment, member_id: @member.id, post_id: @post.id)
      )
      @comment_like = Libertree::Model::CommentLike.create(
        FactoryGirl.attributes_for(:comment_like, comment_id: @comment.id, member_id: @member.id)
      )
      @pool = Libertree::Model::Pool.create(
        FactoryGirl.attributes_for(:pool, member_id: @member.id)
      )
    end

    describe 'req_comment' do
      it 'produces expected xml' do
        expected =<<XML
<comment>
  <id>#{@comment.id}</id>
  <post_id>#{@post.public_id}</post_id>
  <origin>#{@requester.domain}</origin>
  <username>#{@comment.member.username}</username>
  <text>#{@comment.text}</text>
</comment>
XML
        expect( @c.req_comment(@comment) ).to eq(expected.strip)
      end
    end

    describe 'req_comment_delete' do
      it 'produces expected xml' do
        expected =<<XML
<comment-delete>
  <id>1</id>
</comment-delete>
XML
        expect( @c.req_comment_delete(1) ).to eq(expected.strip)
      end
    end

    describe 'req_comment_like' do
      it 'produces expected xml' do
        expected =<<XML
<comment-like>
  <id>#{@comment_like.id}</id>
  <comment_id>#{@comment_like.comment.public_id}</comment_id>
  <origin>#{@comment_like.member.server.domain}</origin>
  <username>#{@comment_like.member.username}</username>
</comment-like>
XML
        expect( @c.req_comment_like(@comment_like) ).to eq(expected.strip)
      end
    end

    describe 'req_comment_like_delete' do
      it 'produces expected xml' do
        expected =<<XML
<comment-like-delete>
  <id>1</id>
</comment-like-delete>
XML
        expect( @c.req_comment_like_delete(1) ).to eq(expected.strip)
      end
    end

    describe 'req_forest' do
      it 'produces expected xml' do
        expected =<<XML
<forest>
  <id>#{@forest.id}</id>
  <name>#{@forest.name}</name>
  <trees>
    <domain>#{@forest.trees[0].domain}</domain>
    <domain>#{@domain}</domain>
  </trees>
</forest>
XML
        expect( @c.req_forest(@forest) ).to eq(expected.strip)
      end
    end

    describe 'req_introduce' do
      it 'produces expected xml' do
        expected =<<XML
<introduce>
  <public_key>#{@public_key}</public_key>
  <contact>#{@contact}</contact>
  <server_name>#{@server_name}</server_name>
</introduce>
XML
        expect( @c.req_introduce ).to eq(expected.strip)
      end
    end

    describe 'req_member' do
      it 'produces expected xml' do
        expected =<<XML
<member>
  <username>#{@member.username}</username>
  <profile>
    <name_display>#{@member.profile.name_display}</name_display>
    <description>foo bar</description>
  </profile>
</member>
XML
        expect( @c.req_member(@member) ).to eq(expected.strip)
      end
    end
    describe 'req_member_delete' do
      it 'produces expected xml' do
        expected =<<XML
<member-delete>
  <username>tester</username>
</member-delete>
XML
        expect( @c.req_member_delete('tester') ).to eq(expected.strip)
      end
    end
    describe 'req_message' do
      it 'produces expected xml' do
        expected =<<XML
<message>
  <username>#{@member3.username}</username>
  <text>#{@message.text}</text>
  <recipients>
    <recipient>
      <username>#{@member.username}</username>
      <origin>#{@member.server.domain}</origin>
    </recipient>
    <recipient>
      <username>#{@member2.username}</username>
      <origin>#{@member2.server.domain}</origin>
    </recipient>
  </recipients>
</message>
XML
        expect( @c.req_message(@message, [@member, @member2]) ).to eq(expected.strip)
      end
    end

    describe 'req_post' do
      it 'produces expected xml' do
        expected =<<XML
<post>
  <username>#{@post.member.username}</username>
  <id>#{@post.id}</id>
  <visibility>#{@post.visibility}</visibility>
  <text>#{@post.text}</text>
</post>
XML
        expect( @c.req_post(@post) ).to eq(expected.strip)
      end
    end

    describe 'req_pool' do
      it 'produces expected xml' do
        expected =<<XML
<pool>
  <username>#{@pool.member.username}</username>
  <id>#{@pool.id}</id>
  <name>#{@pool.name}</name>
</pool>
XML
        expect( @c.req_pool(@pool) ).to eq(expected.strip)
      end
    end

    describe 'req_pool_delete' do
      it 'produces expected xml' do
        expected =<<XML
<pool-delete>
  <username>#{@pool.member.username}</username>
  <id>#{@pool.id}</id>
</pool-delete>
XML
        expect( @c.req_pool_delete(@pool) ).to eq(expected.strip)
      end
    end

    describe 'req_pool_post' do
      it 'produces expected xml' do
        expected =<<XML
<pool-post>
  <username>#{@pool.member.username}</username>
  <pool_id>#{@pool.id}</pool_id>
  <post_id>#{@post.public_id}</post_id>
  <origin>#{@pool.member.server.domain}</origin>
</pool-post>
XML
        expect( @c.req_pool_post(@pool, @post) ).to eq(expected.strip)
      end
    end

    describe 'req_pool_post_delete' do
      it 'produces expected xml' do
        expected =<<XML
<pool-post-delete>
  <username>#{@pool.member.username}</username>
  <pool_id>#{@pool.id}</pool_id>
  <post_id>#{@post.public_id}</post_id>
  <origin>#{@pool.member.server.domain}</origin>
</pool-post-delete>
XML
        expect( @c.req_pool_post_delete(@pool, @post) ).to eq(expected.strip)
      end
    end

    describe 'req_post_delete' do
      it 'produces expected xml' do
        expected =<<XML
<post-delete>
  <id>1</id>
</post-delete>
XML
        expect( @c.req_post_delete(1) ).to eq(expected.strip)
      end
    end

    describe 'req_post_like' do
      it 'produces expected xml' do
        expected =<<XML
<post-like>
  <id>#{@comment_like.id}</id>
  <post_id>#{@post_like.post.public_id}</post_id>
  <origin>#{@post_like.member.server.domain}</origin>
  <username>#{@post_like.member.username}</username>
</post-like>
XML
        expect( @c.req_post_like(@post_like) ).to eq(expected.strip)
      end
    end

    describe 'req_post_like_delete' do
      it 'produces expected xml' do
        expected =<<XML
<post-like-delete>
  <id>1</id>
</post-like-delete>
XML
        expect( @c.req_post_like_delete(1) ).to eq(expected.strip)
      end
    end

  end
end
