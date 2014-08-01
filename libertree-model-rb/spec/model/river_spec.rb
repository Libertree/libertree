# -*- coding: utf-8 -*-
require 'spec_helper'

describe Libertree::Model::River do
  before do
    @account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
  end

  describe '#query_components' do

    def test_one( query, expected )
      river = Libertree::Model::River.create(
        FactoryGirl.attributes_for( :river, label: query, query: query, account_id: @account.id )
      )
      expect( river.query_components.values.flatten ).to match_array(expected)
    end

    def test_exact( query, expected )
      river = Libertree::Model::River.create(
        FactoryGirl.attributes_for( :river, label: query, query: query, account_id: @account.id )
      )
      expect( river.query_components ).to eq(expected)
    end

    it "splits the text of the river's query along spaces into an Array of words" do
      test_one  'test', ['test']
      test_one  'test two', ['test', 'two']
      test_one  'a b c', ['a', 'b', 'c']
      test_one  '1 _ !', ['1', '_', '!']
    end

    it 'treats quoted strings as single components' do
      test_one  %{match "as is"}, [ 'match', 'as is' ]
    end

    it 'ignores unmatched quotes' do
      test_one  %{match "as is}, [ 'match', '"as', 'is' ]
      test_one  %{match "as is" "hey there}, [ 'match', 'as is', '"hey', 'there' ]
      test_one  %{match "as is" hey" there}, [ 'match', 'as is', 'hey"', 'there' ]
    end

    it 'ignores quotes inside words' do
      test_one  %{match a"s is}, [ 'match', 'a"s', 'is' ]
      test_one  %{match a"s i"s}, [ 'match', 'a"s', 'i"s' ]
    end

    it 'treats "minused" quoted strings as single components' do
      test_one  %{match -"as is" yo}, [ 'match', '-as is', 'yo' ]
    end

    it 'treats a term with a quoted argument as a single query component' do
      [
        ':from',
        ':river',
        ':contact-list',
      ].each do |term|
        test_one  %{#{term} "abc"}, [ %{#{term} "abc"}, ]
        test_one  %{#{term} "abc def"}, [ %{#{term} "abc def"}, ]
        test_one  %{abc #{term} "def"}, [ 'abc', %{#{term} "def"}, ]
        test_one  %{abc #{term} "def ghi" jkl}, [ 'abc', %{#{term} "def ghi"}, 'jkl', ]

        test_one  %{-#{term} "abc"}, [ %{-#{term} "abc"}, ]
        test_one  %{-#{term} "abc def"}, [ %{-#{term} "abc def"}, ]
        test_one  %{abc -#{term} "def"}, [ 'abc', %{-#{term} "def"}, ]
        test_one  %{abc -#{term} "def ghi" jkl}, [ 'abc', %{-#{term} "def ghi"}, 'jkl', ]

        test_one  %{+#{term} "abc"}, [ %{+#{term} "abc"}, ]
        test_one  %{+#{term} "abc def"}, [ %{+#{term} "abc def"}, ]
        test_one  %{abc +#{term} "def"}, [ 'abc', %{+#{term} "def"}, ]
        test_one  %{abc +#{term} "def ghi" jkl}, [ 'abc', %{+#{term} "def ghi"}, 'jkl', ]
      end
    end

    it 'treats :visibility ... as a single term' do
      test_one  %{:visibility abc}, [ ':visibility abc', ]
      test_one  %{abc :visibility def}, [ 'abc', ':visibility def', ]
      test_one  %{-:visibility abc}, [ '-:visibility abc', ]
      test_one  %{abc -:visibility def}, [ 'abc', '-:visibility def', ]
      test_one  %{+:visibility abc}, [ '+:visibility abc', ]
      test_one  %{abc +:visibility def}, [ 'abc', '+:visibility def', ]
    end

    it 'treats :word-count < n as a single term' do
      test_one  %{:word-count < 5}, [ ':word-count < 5', ]
      test_one  %{abc :word-count < 5}, [ 'abc', ':word-count < 5', ]
      test_one  %{:word-count < 987}, [ ':word-count < 987', ]
      test_one  %{abc :word-count < 987}, [ 'abc', ':word-count < 987', ]
      test_one  %{:word-count > 5}, [ ':word-count > 5', ]
      test_one  %{abc :word-count > 5}, [ 'abc', ':word-count > 5', ]
      test_one  %{-:word-count < 5}, [ '-:word-count < 5', ]
      test_one  %{abc -:word-count < 5}, [ 'abc', '-:word-count < 5', ]
      test_one  %{+:word-count < 5}, [ '+:word-count < 5', ]
      test_one  %{abc +:word-count < 5}, [ 'abc', '+:word-count < 5', ]
    end

    it 'treats :spring "..." "..." as a single term' do
      test_one  ':spring "Spring Name" "username@treename"', [ ':spring "Spring Name" "username@treename"', ]
      test_one  'foo :spring "Spring Name" "username@treename"', [ 'foo', ':spring "Spring Name" "username@treename"', ]
    end

    it 'treats :via "..." as a single term' do
      test_one  ':via "something or other"', [ ':via "something or other"', ]
      test_one  'foo :via "abcdef"', [ 'foo', ':via "abcdef"', ]
    end

    it 'considers single word terms as static' do
      test_exact 'foo bar baz', {:static => ['foo', 'bar', 'baz'], :dynamic => []}
    end

    it 'considers special terms as dynamic' do
      test_exact 'foo bar baz :word-count > 10', {:static => ['foo', 'bar', 'baz'], :dynamic => [':word-count > 10']}
    end
  end

  describe '#matches_post?' do
    before do
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
      before do
        server = Libertree::Model::Server.create(
          FactoryGirl.attributes_for(:server, name_given: 'remote' )
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
        before do
          @member4.profile.name_display = 'First1 Last1'
          @member4.profile.save
          @member5.profile.name_display = 'First2 Last2'
          @member5.profile.save
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

      it 'does not match posts in pools that have not been sprung' do
        river = Libertree::Model::River.create(
          FactoryGirl.attributes_for( :river, query: %|:spring "My Pool" "#{@member.handle}"|, account_id: @account.id )
        )
        river.matches_post?(@post_private).should be_false
        river.matches_post?(@post_feed).should be_false
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
