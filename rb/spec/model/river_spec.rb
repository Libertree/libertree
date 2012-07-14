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
      river.query_components.should == expected
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

    it 'treats :from "..." as a single term' do
      test_one  %{:from "abc"}, [ ':from "abc"', ]
      test_one  %{:from "abc def"}, [ ':from "abc def"', ]
      test_one  %{abc :from "def"}, [ 'abc', ':from "def"', ]
      test_one  %{abc :from "def ghi" jkl}, [ 'abc', ':from "def ghi"', 'jkl', ]
    end

    it 'treats -:from "..." as a single term' do
      test_one  %{-:from "abc"}, [ '-:from "abc"', ]
      test_one  %{-:from "abc def"}, [ '-:from "abc def"', ]
      test_one  %{abc -:from "def"}, [ 'abc', '-:from "def"', ]
      test_one  %{abc -:from "def ghi" jkl}, [ 'abc', '-:from "def ghi"', 'jkl', ]
    end

    it 'treats +:from "..." as a single term' do
      test_one  %{+:from "abc"}, [ '+:from "abc"', ]
      test_one  %{+:from "abc def"}, [ '+:from "abc def"', ]
      test_one  %{abc +:from "def"}, [ 'abc', '+:from "def"', ]
      test_one  %{abc +:from "def ghi" jkl}, [ 'abc', '+:from "def ghi"', 'jkl', ]
    end

    it 'treats :river "..." as a single term' do
      test_one  %{:river "abc"}, [ ':river "abc"', ]
      test_one  %{:river "abc def"}, [ ':river "abc def"', ]
      test_one  %{abc :river "def"}, [ 'abc', ':river "def"', ]
      test_one  %{abc :river "def ghi" jkl}, [ 'abc', ':river "def ghi"', 'jkl', ]
    end

    it 'treats -:river "..." as a single term' do
      test_one  %{-:river "abc"}, [ '-:river "abc"', ]
      test_one  %{-:river "abc def"}, [ '-:river "abc def"', ]
      test_one  %{abc -:river "def"}, [ 'abc', '-:river "def"', ]
      test_one  %{abc -:river "def ghi" jkl}, [ 'abc', '-:river "def ghi"', 'jkl', ]
    end

    it 'treats +:river "..." as a single term' do
      test_one  %{+:river "abc"}, [ '+:river "abc"', ]
      test_one  %{+:river "abc def"}, [ '+:river "abc def"', ]
      test_one  %{abc +:river "def"}, [ 'abc', '+:river "def"', ]
      test_one  %{abc +:river "def ghi" jkl}, [ 'abc', '+:river "def ghi"', 'jkl', ]
    end
  end

  describe '#matches_post?' do
    before do
      other_account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
      @member = Libertree::Model::Member.create(
        FactoryGirl.attributes_for( :member, :account_id => other_account.id, username: nil )
      )
    end

    def try_one( query, post_text, should_match, post_author = @member )
      river = Libertree::Model::River.create(
        FactoryGirl.attributes_for( :river, label: query, query: query, account_id: @account.id )
      )

      if should_match
        post = Libertree::Model::Post.create(
          FactoryGirl.attributes_for( :post, member_id: post_author.id, text: post_text )
        )
        river.matches_post?(post).should be_true
      else
        post = Libertree::Model::Post.create(
          FactoryGirl.attributes_for( :post, member_id: post_author.id, text: post_text )
        )
        river.matches_post?(post).should be_false
      end

      Libertree::DB.dbh.d  "DELETE FROM river_posts WHERE river_id = ?", river.id
      Libertree::DB.dbh.d  "DELETE FROM rivers WHERE id = ?", river.id
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
        @member2 = Libertree::Model::Member.create(
          FactoryGirl.attributes_for( :member, :account_id => @account2.id, username: nil )
        )
        @account3 = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
        @member3 = Libertree::Model::Member.create(
          FactoryGirl.attributes_for( :member, :account_id => @account3.id, username: nil )
        )
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
          @member5.profile.name_display = 'First2 Last2'
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
  end
end
