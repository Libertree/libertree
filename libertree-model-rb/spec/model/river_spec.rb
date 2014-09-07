# -*- coding: utf-8 -*-
require 'spec_helper'

describe Libertree::Model::River do
  before :all do
    @account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
  end

  describe '#refresh_posts' do
    it 'limits the result set' do
      id = @account.member.id
      (1..25).each do |n|
        Libertree::Model::Post.create(FactoryGirl.attributes_for( :post, member_id: id, text: n ))
      end
      river = Libertree::Model::River.create(
        FactoryGirl.attributes_for( :river, label: ':forest', query: ':forest', account_id: @account.id )
      )
      river.refresh_posts(10)
      expect( river.posts.count ).to eq(10)

      river.refresh_posts(100)
      expect( river.posts({limit: 1000}).count ).to eq([Libertree::Model::Post.count, 100].min)
    end
  end

  describe '#parsed_query' do
    before :all do
      @river = Libertree::Model::River.create(
        FactoryGirl.attributes_for( :river, label: 'parsed-query-river', query: '', account_id: @account.id )
      )
    end

    it 'returns an empty hash for an empty query' do
      expect( @river.parsed_query(true) ).to eq({})
    end

    it 'sorts elements into one of the buckets "negations", "requirements", or "regular"' do
      @river.update(query: '+hello -goodbye sleepy kitty')
      expected = {
        'word' => {
          :negations    => ['goodbye'],
          :requirements => ['hello'],
          :regular      => ['sleepy', 'kitty']
        }
      }
      expect( @river.parsed_query(true) ).to eq(expected)

      @river.update(query: '+hello +world')
      expected = {
        'word' => {
          :negations    => [],
          :requirements => ['hello', 'world'],
          :regular      => []
        }
      }
      expect( @river.parsed_query(true) ).to eq(expected)
    end

    it 'identifies and groups flags' do
      @river.update(query: '+:forest -:tree :unread :liked :commented :subscribed')
      expected = {
        'flag' => {
          :negations    => ['tree'],
          :requirements => ['forest'],
          :regular      => ['unread', 'liked', 'commented', 'subscribed']
        }
      }
      expect( @river.parsed_query(true) ).to eq(expected)
    end

    it 'identifies and groups phrase, via, visibility, word-count and tag' do
      @river.update(query: '+"hello world" -"goodbye ruby tuesday" "sleepy kitty"')
      expected = {
        'phrase' => {
          :negations    => ['goodbye ruby tuesday'],
          :requirements => ['hello world'],
          :regular      => ['sleepy kitty']
        }
      }
      expect( @river.parsed_query(true) ).to eq(expected)

      @river.update(query: '+:via "api" -:via "source of spam" :via "tree"')
      expected = {
        'via' => {
          :negations    => ['source of spam'],
          :requirements => ['api'],
          :regular      => ['tree']
        }
      }
      expect( @river.parsed_query(true) ).to eq(expected)

      @river.update(query: '+:visibility tree -:visibility forest :visibility internet')
      expected = {
        'visibility' => {
          :negations    => ['forest'],
          :requirements => ['tree'],
          :regular      => ['internet']
        }
      }
      expect( @river.parsed_query(true) ).to eq(expected)

      @river.update(query: '+:word-count >12 -:word-count < 20 :word-count > 100')
      expected = {
        'word-count' => {
          :negations    => ['< 20'],
          :requirements => ['>12'],
          :regular      => ['> 100']
        }
      }
      expect( @river.parsed_query(true) ).to eq(expected)


      @river.update(query: '+#awesome -#lame #whatever')
      expected = {
        'tag' => {
          :negations    => ['lame'],
          :requirements => ['awesome'],
          :regular      => ['whatever']
        }
      }
      expect( @river.parsed_query(true) ).to eq(expected)
    end

    it 'looks up a member to verify :from query' do
      user = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
      @river.update(query: ":from \"#{user.member.handle}\"")
      expected = {
        'from' => {
          :negations    => [],
          :requirements => [],
          :regular      => [user.member.id]
        }
      }
      expect( @river.parsed_query(true) ).to eq(expected)
    end

    it 'returns an empty hash for an invalid :from query' do
      @river.update(query: ':from "thisuserdoesnotexist@thishostdoesnotexist"')
      expect( @river.parsed_query(true) ).to eq({})
    end

    it 'merges the parsed query of a referenced river into the current query' do
      pending 'this requires more rigorous logic processing'
      river = Libertree::Model::River.create( query: 'some whatever',
                                              label: 'river-spec-included-river',
                                              account_id: @account.id )
      @river.update(query: "some terms :river \"#{river.label}\"")
      expected = {
        'word' => {
          :negations    => [],
          :requirements => [],
          :regular      => ['some', 'terms', 'whatever']
        }
      }
      expect( @river.parsed_query(true) ).to eq(expected)
    end

    it 'ignores self-referential river queries' do
      @river.update(query: ":river \"#{@river.label}\"")
      expect( @river.parsed_query(true) ).to eq({})
    end

    it 'looks up a contact list to verify :contact-list query' do
      a = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
      member1 = a.member
      b = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
      member2 = b.member
      list = Libertree::Model::ContactList.create( account_id: @river.account.id,
                                                   name: "river-spec-list-name1" )
      list << member1
      list << member2

      @river.update(query: ':contact-list "river-spec-list-name1"')
      expected = {
        'contact-list' => {
          :negations    => [],
          :requirements => [],
          :regular      => [[list.id, list.member_ids]]
        }
      }
      expect( @river.parsed_query(true) ).to eq(expected)
    end

    it 'ignores empty contact list' do
      list = Libertree::Model::ContactList.create( account_id: @river.account.id,
                                                   name: "river-spec-list-name2" )
      @river.update(query: ':contact-list "river-spec-list-name2"')
      expect( @river.parsed_query(true) ).to eq({})
    end

    it 'looks up a spring to verify :spring query' do
      user = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
      spring = Libertree::Model::Pool.create( member_id: user.member.id,
                                              sprung: true,
                                              name: 'river-spec-spring')
      @river.update(query: ":spring \"river-spec-spring\" \"#{user.member.handle}\"")
      expected = {
        'spring' => {
          :negations    => [],
          :requirements => [],
          :regular      => [spring]
        }
      }
      expect( @river.parsed_query(true) ).to eq(expected)
    end

    it 'ignores the :spring query if the pool does not exist or is not sprung' do
      user = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
      spring = Libertree::Model::Pool.create( member_id: user.member.id,
                                              sprung: false,
                                              name: 'river-spec-pool')
      @river.update(query: ":spring \"river-spec-pool\" \"#{user.member.handle}\"")
      expect( @river.parsed_query(true) ).to eq({})

      @river.update(query: ":spring \"river-spec-pool-does-not-exist\" \"#{user.member.handle}\"")
      expect( @river.parsed_query(true) ).to eq({})
    end

    it 'treats words with special characters as phrases' do
      @river.update(query: "http://google.com duckduckgo o'brian stop&go")
      expected = {
        'phrase' => {
          :negations    => [],
          :requirements => [],
          :regular      => ['http://google.com', 'o\'brian', 'stop&go']
        },
        'word' => {
          :negations    => [],
          :requirements => [],
          :regular      => ['duckduckgo']
        }
      }
      expect( @river.parsed_query(true) ).to eq(expected)
    end

    it 'does not mind long whitespace stretches' do
      @river.update(query: "     hello    bye   ")
      expected = {
        'word' => {
          :negations    => [],
          :requirements => [],
          :regular      => ['hello', 'bye']
        }
      }
      expect( @river.parsed_query(true) ).to eq(expected)
    end

    it 'does not mind new line or odd whitespace characters' do
      @river.update(query: "\t  \fhello  \n  bye\r\nciao\rhi   ")
      expected = {
        'word' => {
          :negations    => [],
          :requirements => [],
          :regular      => ['hello', 'bye', 'ciao', 'hi']
        }
      }
      expect( @river.parsed_query(true) ).to eq(expected)
    end
  end

  describe '#matches_post?' do
    before do
      @account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
      other_account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
      @member = other_account.member
    end

    def try_one( query, post_text, should_match, post_author = @member )
      river = Libertree::Model::River.create(
        FactoryGirl.attributes_for( :river, label: query, query: query, account_id: @account.id )
      )

      post = Libertree::Model::Post.create(
        FactoryGirl.attributes_for( :post, member_id: post_author.id, text: post_text )
      )
      river.matches_post?(post).should == should_match

      river.delete_cascade
      post.delete_cascade
    end

    it 'matches in basic cases' do
      try_one  'test', 'no match', false
      try_one  'test', 'test', true
      try_one  'test', 'and test', true
    end

    it 'only matches whole words' do
      try_one  'test', 'testing', false
      try_one  'test', 'It was tested.', false
    end

    it 'matches non-ASCII characters' do
      try_one  '♲', '♲ Friendica reshare', true
      try_one  '♲', 'This is a ♲ Friendica reshare', true
      try_one  '♲', 'Friendica reshare', false
      try_one  '-♲', '♲ Friendica reshare', false
      try_one  '-♲', 'This is a ♲ Friendica reshare', false
      try_one  '-♲', 'Friendica reshare', true
    end

    it 'avoids matching when a minus term matches' do
      try_one  'test -foo', 'This is where we test foo', false
    end

    it 'requires every term with a plus in front' do
      try_one  'test +foo', 'This is a test.', false
      try_one  'test +foo', 'This is a test foo.', true
      try_one  'test +foo +bar', 'This is a test foo.', false
      try_one  'test +foo +bar', 'This is a test bar.', false
      try_one  'test +foo +bar', 'This is a test foo bar.', true
      try_one  'test +foo +bar', 'This is bar a test foo.', true
    end

    context 'given two different local posters' do
      before do
        @account2 = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
        @member2 = @account2.member
        @account3 = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
        @member3 = @account3.member
      end

      it 'matches :from "account username"' do
        try_one  %{:from "#{@account2.username}"}, 'Post by the second account.', true, @member2
        try_one  %{:from "#{@account3.username}"}, 'Post by the second account.', false, @member2
        try_one  %{:from "#{@account2.username}"}, 'Post by the third account.', false, @member3
        try_one  %{:from "#{@account3.username}"}, 'Post by the third account.', true, @member3
      end
    end

    context 'given two different remote posters' do
      before :all do
        server = Libertree::Model::Server.create(
          FactoryGirl.attributes_for(:server, domain: 'remote' )
        )
        @member4 = Libertree::Model::Member.create(
          FactoryGirl.attributes_for( :member, username: 'poster3', server_id: server.id )
        )
        @member5 = Libertree::Model::Member.create(
          FactoryGirl.attributes_for( :member, username: 'poster4', server_id: server.id )
        )
      end

      it 'matches :from "member handle"' do
        try_one  ':from "poster3@remote"', 'Post by fourth member.', true, @member4
        try_one  ':from "poster3@remote"', 'Post by fifth member.', false, @member5
        try_one  ':from "poster4@remote"', 'Post by fourth member.', false, @member4
        try_one  ':from "poster4@remote"', 'Post by fifth member.', true, @member5
      end

      context 'with display names' do
        before :all do
          @member4.profile.update(name_display: 'First1 Last1')
          @member5.profile.update(name_display: 'First2 Last2')
        end

        it 'matches :from "member display name"' do
          try_one  ':from "First1 Last1"', 'Post by fourth member.', true, @member4
          try_one  ':from "First1 Last1"', 'Post by fifth member.', false, @member5
          try_one  ':from "First2 Last2"', 'Post by fourth member.', false, @member4
          try_one  ':from "First2 Last2"', 'Post by fifth member.', true, @member5
        end

        it 'does not match -:from "member display name"' do
          try_one  '-:from "First1 Last1"', 'Post by fourth member.', false, @member4
          try_one  '-:from "First1 Last1"', 'Post by fifth member.', true, @member5
          try_one  '-:from "First2 Last2"', 'Post by fourth member.', true, @member4
          try_one  '-:from "First2 Last2"', 'Post by fifth member.', false, @member5
        end

        it 'requires authorship with +:from' do
          try_one  '+:from "First1 Last1"', 'Post by fourth member.', true, @member4
          try_one  '+:from "First1 Last1"', 'Post by fifth member.', false, @member5
          try_one  '+:from "First1 Last1" foo', 'Post by fourth member.', false, @member4
          try_one  '+:from "First1 Last1" foo', 'Post foo by fifth member.', false, @member5
          try_one  '+:from "First1 Last1" foo', 'Post foo by fourth member.', true, @member4
        end
      end
    end

    it 'matches quoted text' do
      try_one  '"foo bar"', 'foo and bar', false
      try_one  '"foo bar"', 'bar foo', false
      try_one  '"foo bar"', 'foo bar', true
      try_one  '"foo bar"', 'and foo bar', true
      try_one  '"foo bar"', 'foo bar and', true
      try_one  '"foo bar"', 'andfoo bar', false
      try_one  '"foo bar"', 'foo barand', false
      try_one  '"foo bar"', '.foo bar.', true
      try_one  '"foo bar"', 'will foo bar.', true

      try_one  '"foo bar" baz', 'foo and bar baz', true

      try_one  '-"foo bar"', 'foo and bar', true
      try_one  'hey -"foo bar"', 'foo and bar hey', true
      try_one  'hey -"foo bar"', 'foo bar hey', false

      try_one  '+"foo bar"', 'foo and bar', false
      try_one  '+"foo bar"', 'foo bar', true
      try_one  '+"foo bar" baz', 'foo bar bleh', false
      try_one  '+"foo bar" baz', 'foo bar baz', true
      try_one  '+"foo bar" baz bleh', 'foo bar baz', true
      try_one  '+"foo bar" baz bleh', 'foo bar bleh', true
    end

    it "matches posts liked by the river's account" do
      river = Libertree::Model::River.create(
        FactoryGirl.attributes_for( :river, label: ':liked', query: ':liked', account_id: @account.id )
      )
      post = Libertree::Model::Post.create(
        FactoryGirl.attributes_for( :post, member_id: @member.id, text: 'test post' )
      )

      river.matches_post?(post).should be_false

      Libertree::Model::PostLike.create(
        FactoryGirl.attributes_for(
          :post_like,
          'member_id' => @account.member.id,
          'post_id'   => post.id
        )
      )

      river.matches_post?(post).should be_true

      river.delete_cascade
    end

    it "matches posts commented on by the river's account" do
      river = Libertree::Model::River.create(
        FactoryGirl.attributes_for( :river, label: ':commented', query: ':commented', account_id: @account.id )
      )
      post = Libertree::Model::Post.create(
        FactoryGirl.attributes_for( :post, member_id: @member.id, text: 'test post' )
      )

      river.matches_post?(post).should be_false

      Libertree::Model::Comment.create(
        FactoryGirl.attributes_for( :comment, member_id: @account.member.id, post_id: post.id, text: 'test comment' )
      )

      river.matches_post?(post).should be_true

      river.delete_cascade
    end

    it "matches posts subscribed to by the river's account" do
      river = Libertree::Model::River.create(
        FactoryGirl.attributes_for( :river, label: ':subscribed', query: ':subscribed', account_id: @account.id )
      )
      post = Libertree::Model::Post.create(
        FactoryGirl.attributes_for( :post, member_id: @member.id, text: 'test post' )
      )

      river.matches_post?(post).should be_false

      @account.subscribe_to post

      river.matches_post?(post).should be_true

      river.delete_cascade
    end

    it 'allows composition of rivers via :river term' do
      Libertree::Model::River.create(
        FactoryGirl.attributes_for( :river, label: 'foo', query: 'foo', account_id: @account.id )
      )
      try_one  ':river "foo"', 'foo bar', true
      try_one  ':river "foo"', 'bar', false

      Libertree::Model::River.create(
        FactoryGirl.attributes_for( :river, label: 'Multi-word Label', query: 'foo2', account_id: @account.id )
      )
      try_one  ':river "Multi-word Label"', 'foo2 bar', true
      try_one  ':river "Multi-word Label"', 'bar', false

      Libertree::Model::River.create(
        FactoryGirl.attributes_for( :river, label: 'bar', query: 'bar', account_id: @account.id )
      )
      try_one  'foo :river "bar"', 'foo bar', true
      try_one  'foo :river "bar"', 'foo baz', true
      try_one  'foo :river "bar"', 'bin baz', false
      try_one  'foo -:river "bar"', 'foo bar', false
      try_one  'foo -:river "bar"', 'foo baz', true
      try_one  'foo +:river "bar"', 'foo bar', true
      try_one  'foo +:river "bar"', 'foo baz', false
      try_one  'foo baz +:river "bar"', 'foo baz', false
      try_one  'foo baz +:river "bar"', 'foo baz bar', true
      try_one  'foo baz +:river "bar"', 'foo bar', true
      try_one  'foo baz +:river "bar"', 'baz bar', true

      Libertree::Model::River.create(
        FactoryGirl.attributes_for( :river, label: 'level1', query: 'level1', account_id: @account.id )
      )
      Libertree::Model::River.create(
        FactoryGirl.attributes_for( :river, label: 'composed', query: ':river "level1" level2', account_id: @account.id )
      )
      try_one  'foo :river "composed"', 'foo', true
      try_one  'foo :river "composed"', 'level1', true
      try_one  'foo :river "composed"', 'level2', true
      try_one  'foo :river "composed"', 'foo level2', true
      try_one  'foo :river "composed"', 'none', false
    end

    context 'given a river set to be appended to all other rivers' do
      before :each do
        @river = Libertree::Model::River.create(
          FactoryGirl.attributes_for( :river, label: 'global', query: 'foo', account_id: @account.id, appended_to_all: true )
        )
      end

      it "appends that river's query to other rivers" do
        try_one  'bar', 'foo', true
        try_one  'bar', 'baz', false
      end

      after :each do
        @river.delete_cascade
      end
    end

    context 'given some liked and commented on posts' do
      before :each do
        @post_liked = Libertree::Model::Post.create(
          FactoryGirl.attributes_for( :post, member_id: @member.id, text: 'test post' )
        )
        Libertree::Model::PostLike.create(
          FactoryGirl.attributes_for(
            :post_like,
            'member_id' => @account.member.id,
            'post_id'   => @post_liked.id
          )
        )

        @post_commented = Libertree::Model::Post.create(
          FactoryGirl.attributes_for( :post, member_id: @member.id, text: 'test post' )
        )
        Libertree::Model::Comment.create(
          FactoryGirl.attributes_for( :comment, member_id: @account.member.id, post_id: @post_commented.id, text: 'test comment' )
        )

        @post_liked_and_commented = Libertree::Model::Post.create(
          FactoryGirl.attributes_for( :post, member_id: @member.id, text: 'test post' )
        )
        Libertree::Model::PostLike.create(
          FactoryGirl.attributes_for(
            :post_like,
            'member_id' => @account.member.id,
            'post_id'   => @post_liked_and_commented.id
          )
        )
        Libertree::Model::Comment.create(
          FactoryGirl.attributes_for( :comment, member_id: @account.member.id, post_id: @post_liked_and_commented.id, text: 'test comment' )
        )
      end

      it 'matches correctly with combinations of :liked and :commented' do
        river = Libertree::Model::River.create(
          FactoryGirl.attributes_for( :river, label: 'r1', query: ':liked :commented', account_id: @account.id )
        )
        river.matches_post?(@post_commented).should be_true
        river.matches_post?(@post_liked).should be_true
        river.matches_post?(@post_liked_and_commented).should be_true

        river = Libertree::Model::River.create(
          FactoryGirl.attributes_for( :river, label: 'r2', query: '+:liked +:commented', account_id: @account.id )
        )
        river.matches_post?(@post_commented).should be_false
        river.matches_post?(@post_liked).should be_false
        river.matches_post?(@post_liked_and_commented).should be_true

        river = Libertree::Model::River.create(
          FactoryGirl.attributes_for( :river, label: 'r3', query: '+:liked :commented', account_id: @account.id )
        )
        river.matches_post?(@post_commented).should be_false
        river.matches_post?(@post_liked).should be_false
        river.matches_post?(@post_liked_and_commented).should be_true

        river = Libertree::Model::River.create(
          FactoryGirl.attributes_for( :river, label: 'r3', query: '+:liked -:commented', account_id: @account.id )
        )
        river.matches_post?(@post_commented).should be_false
        river.matches_post?(@post_liked).should be_true
        river.matches_post?(@post_liked_and_commented).should be_false
      end
    end

    context 'given the river owner has some contact lists' do
      before :each do
        account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
        @contact1 = account.member
        account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
        @contact2 = account.member
        account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
        @contact3 = account.member
        account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
        @contact4 = account.member

        @list1 = Libertree::Model::ContactList.create(
          FactoryGirl.attributes_for( :contact_list, account_id: @account.id, name: 'List 1' )
        )
        @list1.members = [ @contact1.id, @contact2.id, ]
        @list1.save
        @list2 = Libertree::Model::ContactList.create(
          FactoryGirl.attributes_for( :contact_list, account_id: @account.id, name: 'List 2' )
        )
        @list2.members = [ @contact3.id, @contact4.id, ]
        @list2.save

        @post1a = Libertree::Model::Post.create(
          FactoryGirl.attributes_for( :post, member_id: @contact1.id, text: 'test post' )
        )
        @post1b = Libertree::Model::Post.create(
          FactoryGirl.attributes_for( :post, member_id: @contact1.id, text: 'test post' )
        )
        @post2a = Libertree::Model::Post.create(
          FactoryGirl.attributes_for( :post, member_id: @contact2.id, text: 'test post' )
        )
        @post2b = Libertree::Model::Post.create(
          FactoryGirl.attributes_for( :post, member_id: @contact2.id, text: 'test post' )
        )
        @post3a = Libertree::Model::Post.create(
          FactoryGirl.attributes_for( :post, member_id: @contact3.id, text: 'test post' )
        )
        @post3b = Libertree::Model::Post.create(
          FactoryGirl.attributes_for( :post, member_id: @contact3.id, text: 'test post' )
        )
        @post4a = Libertree::Model::Post.create(
          FactoryGirl.attributes_for( :post, member_id: @contact4.id, text: 'test post' )
        )
        @post4b = Libertree::Model::Post.create(
          FactoryGirl.attributes_for( :post, member_id: @contact4.id, text: 'test post' )
        )

        account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
        @non_contact = account.member
        @post_other = Libertree::Model::Post.create(
          FactoryGirl.attributes_for( :post, member_id: @non_contact.id, text: 'test post' )
        )
      end

      it 'matches posts by people in those contact lists, and not posts by people not in those contact lists' do
        river = Libertree::Model::River.create(
          FactoryGirl.attributes_for( :river, query: ':contact-list "List 1"', account_id: @account.id )
        )
        river.matches_post?(@post1a).should be_true
        river.matches_post?(@post1b).should be_true
        river.matches_post?(@post2a).should be_true
        river.matches_post?(@post2b).should be_true
        river.matches_post?(@post3a).should be_false
        river.matches_post?(@post3b).should be_false
        river.matches_post?(@post4a).should be_false
        river.matches_post?(@post4b).should be_false
        river.matches_post?(@post_other).should be_false

        river = Libertree::Model::River.create(
          FactoryGirl.attributes_for( :river, query: ':contact-list "List 2"', account_id: @account.id )
        )
        river.matches_post?(@post1a).should be_false
        river.matches_post?(@post1b).should be_false
        river.matches_post?(@post2a).should be_false
        river.matches_post?(@post2b).should be_false
        river.matches_post?(@post3a).should be_true
        river.matches_post?(@post3b).should be_true
        river.matches_post?(@post4a).should be_true
        river.matches_post?(@post4b).should be_true
        river.matches_post?(@post_other).should be_false

        river = Libertree::Model::River.create(
          FactoryGirl.attributes_for( :river, query: 'test +:contact-list "List 1"', account_id: @account.id )
        )
        river.matches_post?(@post1a).should be_true
        river.matches_post?(@post1b).should be_true
        river.matches_post?(@post2a).should be_true
        river.matches_post?(@post2b).should be_true
        river.matches_post?(@post3a).should be_false
        river.matches_post?(@post3b).should be_false
        river.matches_post?(@post4a).should be_false
        river.matches_post?(@post4b).should be_false
        river.matches_post?(@post_other).should be_false

        river = Libertree::Model::River.create(
          FactoryGirl.attributes_for( :river, query: 'test -:contact-list "List 1"', account_id: @account.id )
        )
        river.matches_post?(@post1a).should be_false
        river.matches_post?(@post1b).should be_false
        river.matches_post?(@post2a).should be_false
        river.matches_post?(@post2b).should be_false
        river.matches_post?(@post3a).should be_true
        river.matches_post?(@post3b).should be_true
        river.matches_post?(@post4a).should be_true
        river.matches_post?(@post4b).should be_true
        river.matches_post?(@post_other).should be_true
      end
    end

    context 'given some posts with varying visibilities' do
      before :each do
        @post_internet = Libertree::Model::Post.create(
          FactoryGirl.attributes_for( :post, member_id: @member.id, text: 'test post', visibility: 'internet' )
        )
        @post_forest = Libertree::Model::Post.create(
          FactoryGirl.attributes_for( :post, member_id: @member.id, text: 'test post', visibility: 'forest' )
        )
      end

      it 'matches posts based on their visibility' do
        river = Libertree::Model::River.create(
          FactoryGirl.attributes_for( :river, query: 'test +:visibility internet', account_id: @account.id )
        )
        river.matches_post?(@post_internet).should be_true
        river.matches_post?(@post_forest).should be_false

        river = Libertree::Model::River.create(
          FactoryGirl.attributes_for( :river, query: 'test +:visibility forest', account_id: @account.id )
        )
        river.matches_post?(@post_internet).should be_false
        river.matches_post?(@post_forest).should be_true

        river = Libertree::Model::River.create(
          FactoryGirl.attributes_for( :river, query: 'test -:visibility internet', account_id: @account.id )
        )
        river.matches_post?(@post_internet).should be_false
        river.matches_post?(@post_forest).should be_true
      end
    end

    context 'given some posts with varying numbers of words' do
      before :each do
        @few_words = 'few words'
        @several_words = 'There are several words in this post.'
        @many_words = %{
          Lorem ipsum dolor sit amet, consectetur adipiscing elit.  Donec a diam lectus.
          Sed sit amet ipsum mauris.  Maecenas congue ligula ac quam viverra nec
          consectetur ante hendrerit.  Donec et mollis dolor.  Praesent et diam eget
          libero egestas mattis sit amet vitae augue.
        }
      end

      it 'matches posts based on the number of words in them' do
        try_one  ':word-count < 4', @few_words, true
        try_one  ':word-count < 4', @several_words, false
        try_one  ':word-count < 4', @many_words, false
        try_one  ':word-count < 12', @few_words, true
        try_one  ':word-count < 12', @several_words, true
        try_one  ':word-count < 12', @many_words, false
        try_one  ':word-count < 100', @few_words, true
        try_one  ':word-count < 100', @several_words, true
        try_one  ':word-count < 100', @many_words, true
        try_one  ':word-count > 4', @few_words, false
        try_one  ':word-count > 4', @several_words, true
        try_one  ':word-count > 4', @many_words, true
        try_one  ':word-count > 12', @few_words, false
        try_one  ':word-count > 12', @several_words, false
        try_one  ':word-count > 12', @many_words, true
        try_one  ':word-count > 100', @few_words, false
        try_one  ':word-count > 100', @several_words, false
        try_one  ':word-count > 100', @many_words, false
      end
    end

    context 'given some posts in some pools' do
      before :each do
        @pool_private = Libertree::Model::Pool.create(
          FactoryGirl.attributes_for( :pool, member_id: @member.id, name: 'My Pool' )
        )
        @spring = Libertree::Model::Pool.create(
          FactoryGirl.attributes_for( :pool, member_id: @member.id, name: 'Post Feed', sprung: true )
        )

        @post_private = Libertree::Model::Post.create(
          FactoryGirl.attributes_for( :post, member_id: @member.id, text: 'private' )
        )
        @post_feed = Libertree::Model::Post.create(
          FactoryGirl.attributes_for( :post, member_id: @member.id, text: 'feed' )
        )

        @pool_private << @post_private
        @spring << @post_feed
      end

      it 'matches posts in sprung pools' do
        river = Libertree::Model::River.create(
          FactoryGirl.attributes_for( :river, query: %|:spring "Post Feed" "#{@member.handle}"|, account_id: @account.id )
        )
        river.matches_post?(@post_private).should be_false
        river.matches_post?(@post_feed).should be_true
      end
    end

    context 'given some posts from remote sources' do
      before :each do
        @post_from_script = Libertree::Model::Post.create(
          FactoryGirl.attributes_for( :post, member_id: @member.id, text: 'test post', via: 'some-script.pl' )
        )
        @post_from_friendica = Libertree::Model::Post.create(
          FactoryGirl.attributes_for( :post, member_id: @member.id, text: 'test post', via: 'Some Friendica Installation' )
        )
        @post_local = Libertree::Model::Post.create(
          FactoryGirl.attributes_for( :post, member_id: @member.id, text: 'test post' )
        )
      end

      it 'matches posts based on their source' do
        river = Libertree::Model::River.create(
          FactoryGirl.attributes_for( :river, query: 'test +:via "foo"', account_id: @account.id )
        )
        river.matches_post?(@post_from_script).should be_false
        river.matches_post?(@post_from_friendica).should be_false
        river.matches_post?(@post_local).should be_false

        river = Libertree::Model::River.create(
          FactoryGirl.attributes_for( :river, query: 'test +:via "some-script.pl"', account_id: @account.id )
        )
        river.matches_post?(@post_from_script).should be_true
        river.matches_post?(@post_from_friendica).should be_false
        river.matches_post?(@post_local).should be_false

        river = Libertree::Model::River.create(
          FactoryGirl.attributes_for( :river, query: 'test +:via "Some Friendica Installation"', account_id: @account.id )
        )
        river.matches_post?(@post_from_script).should be_false
        river.matches_post?(@post_from_friendica).should be_true
        river.matches_post?(@post_local).should be_false

        river = Libertree::Model::River.create(
          FactoryGirl.attributes_for( :river, query: 'test -:via "foo"', account_id: @account.id )
        )
        river.matches_post?(@post_from_script).should be_true
        river.matches_post?(@post_from_friendica).should be_true
        river.matches_post?(@post_local).should be_true

        river = Libertree::Model::River.create(
          FactoryGirl.attributes_for( :river, query: 'test -:via "some-script.pl"', account_id: @account.id )
        )
        river.matches_post?(@post_from_script).should be_false
        river.matches_post?(@post_from_friendica).should be_true
        river.matches_post?(@post_local).should be_true

        river = Libertree::Model::River.create(
          FactoryGirl.attributes_for( :river, query: 'test -:via "Some Friendica Installation"', account_id: @account.id )
        )
        river.matches_post?(@post_from_script).should be_true
        river.matches_post?(@post_from_friendica).should be_false
        river.matches_post?(@post_local).should be_true
      end
    end
  end
end
