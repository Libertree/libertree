# encoding: utf-8
require 'spec_helper'

describe Libertree::Model::Post do
  before do
    @account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
    @member = @account.member
  end

  def new_post(text)
    @post = Libertree::Model::Post.create(
      FactoryGirl.attributes_for( :post, member_id: @member.id, text: text )
    )
  end

  context 'with the Beatles and a post mentioning all but Paul' do
    before :all do
      @john, @paul, @george, @ringo = [ 'john', 'paul', 'george', 'ringo'].map do |name|
        Libertree::Model::Account.create( username: name, password_encrypted: 'p' )
      end
      Libertree::Model::Server.own_domain = "localhost.net"
    end

    describe '#mentioned_accounts' do
      it 'returns all accounts but Paul\'s' do
        new_post "@john and paul@paul went to see @george but found @ringo@localhost.net."
        expect(@post.mentioned_accounts).to match_array([@john, @george, @ringo])
      end

      it 'should ignore mention of the post author' do
        new_post "@#{@account.username}: I'm talking to myself!"
        expect(@post.mentioned_accounts).not_to include(@account)
      end

      it 'should ignore repeated mentions' do
        new_post "@paul @paul @paul!"
        expect(@post.mentioned_accounts).to eq([@paul])
      end
    end

    describe '#notify_mentioned' do
      before :each do
        new_post "@ringo: are you still playing the drums?"
      end
      it 'should create a notification for the mentioned account' do
        before = @ringo.notifications.count
        @post.notify_mentioned

        # fetch account again to invalidate cache
        # TODO: shouldn't this be done in account.notify_about ?
        @ringo = Libertree::Model::Account[ username: 'ringo' ]
        expect(@ringo.notifications.count).to eq (before + 1)

        subject = @ringo.notifications[0].subject
        expect(subject).to be_kind_of Libertree::Model::Post
        expect(subject).to eq @post
      end
    end
  end

  describe '#glimpse' do
    context 'when the text is short' do
      before :each do
        new_post 'Short text.'
      end
      it 'is all the text' do
        @post.glimpse.should == 'Short text.'
      end
    end

    context 'when the text is long' do
      before :each do
        new_post 'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec a diam lectus. Sed sit amet ipsum mauris. Maecenas congue ligula ac quam viverra nec consectetur ante hendrerit.'
      end
      it 'is only some of the beginning of the text' do
        @post.glimpse.should == 'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Don...'
      end
    end
  end

  describe '#with_tags' do
    context 'expecting posts tagged with #test' do
      opts = { :tag => 'test' }
      it 'returns posts that are tagged with #test' do
        post = new_post 'This is tagged with #test.'
        Libertree::Model::Post.with_tag( opts ).should include(post)
      end

      it 'does not return posts that are tagged with #testing' do
        post = new_post 'This is tagged with #testing.'
        Libertree::Model::Post.with_tag( opts ).should_not include(post)
      end

      it 'does not return posts that contain "invalid#test"' do
        post = new_post 'This is not tagged invalid#test.'
        Libertree::Model::Post.with_tag( opts ).should_not include(post)
      end

      it 'matches posts tagged at the beginning of the post' do
        post = new_post '#test or not to test.'
        Libertree::Model::Post.with_tag( opts ).should include(post)
      end

      it 'matches posts tagged at the beginning of a line' do
        post = new_post "nothing special\n#test or not to test."
        Libertree::Model::Post.with_tag( opts ).should include(post)
      end

      it 'matches posts tagged in parentheses' do
        post = new_post "(#test)"
        Libertree::Model::Post.with_tag( opts ).should include(post)
      end
    end

    context 'with unicode tags' do
      opts = { :tag => '你好' }
      it 'returns posts that are tagged with #你好' do
        post = new_post 'This is tagged with #你好.'
        Libertree::Model::Post.with_tag( opts ).should include(post)
      end

      it 'returns posts that end with the tag #你好' do
        post = new_post 'This is tagged with #你好'
        Libertree::Model::Post.with_tag( opts ).should include(post)
      end

      it 'returns posts that include the tag #你好' do
        post = new_post 'This is tagged with #你好 and nothing else'
        Libertree::Model::Post.with_tag( opts ).should include(post)
      end

      it 'does not return posts that are tagged with #你好吗' do
        post = new_post 'This is tagged with #你好吗.'
        Libertree::Model::Post.with_tag( opts ).should_not include(post)
      end

      it 'does not return posts that contain "好#你好"' do
        post = new_post 'This is not tagged 好#你好.'
        Libertree::Model::Post.with_tag( opts ).should_not include(post)
      end

      it 'does not return posts that contain only "你好"' do
        post = new_post 'This is not tagged 你好'
        Libertree::Model::Post.with_tag( opts ).should_not include(post)
      end
    end

    context 'with accented character tags' do
      opts = { :tag => 'José' }
      it 'returns posts that are tagged with #José' do
        post = new_post 'This is tagged with #José.'
        Libertree::Model::Post.with_tag( opts ).should include(post)
      end

      it 'does not return posts that are tagged with #Josém' do
        post = new_post 'This is tagged with #Josém.'
        Libertree::Model::Post.with_tag( opts ).should_not include(post)
      end

      it 'does not return posts that contain "no#José"' do
        post = new_post 'This is not tagged no#José.'
        Libertree::Model::Post.with_tag( opts ).should_not include(post)
      end
    end
  end

  describe '.search' do
    before :each do
      Libertree::DB.dbh.execute 'TRUNCATE posts CASCADE'
      @p1 = new_post 'This is a test post.'
      @p2 = new_post 'This is also a test post.'
      @p3 = new_post 'This is a post.'
      @p4 = new_post 'That is a comment.'
      @p5 = new_post 'me and my hat'
      @p6 = new_post "'Twas brillig, and the slithy toves Did gyre and gimble in the wabe"
      @p7 = new_post 'This was not good.  However, that is.'
    end

    # Expect the given query to return the given Array of Posts (in any order).
    def expect_search_match(query, posts)
      results = Libertree::Model::Post.search(query)
      expect(posts - results).to be_empty, "Expected #{query.inspect} to also match #{(posts-results).map(&:text)}"
      expect(results - posts).to be_empty, "Expected #{query.inspect} not to match #{(results-posts).map(&:text)}"
    end

    it 'matches posts to single word queries' do
      expect_search_match 'This', [ @p1, @p2, @p3, @p7 ]
      expect_search_match 'is', [ @p1, @p2, @p3, @p4, @p7 ]
      expect_search_match 'test', [ @p1, @p2 ]
      expect_search_match 'test', [ @p1, @p2 ]
      expect_search_match 'That', [ @p4, @p7 ]
      expect_search_match 'me', [ @p5 ]
      expect_search_match 'Me', [ @p5 ]
      expect_search_match 'ME', [ @p5 ]
      expect_search_match 'Twas', [ @p6 ]
    end

    it 'matches posts to multi-word queries' do
      expect_search_match 'This is', [ @p1, @p2, @p3, @p7 ]
      expect_search_match 'is this', [ @p1, @p2, @p3, @p7 ]
      expect_search_match 'that is', [ @p4, @p7 ]
      expect_search_match 'post this', [ @p1, @p2, @p3 ]
    end
  end

  describe 'mark_as_unread_by_all' do
    before :all do
      @john, @paul, @george, @ringo = [ 'john', 'paul', 'george', 'ringo'].map do |name|
        Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
      end
      @account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
      @member = @account.member
      new_post "read post"
    end

    it 'leaves the post untouched for excluded accounts' do
      [ @john, @paul, @george, @ringo ].each do |account|
        @post.mark_as_read_by account
      end

      @post.mark_as_unread_by_all except: [@ringo]

      expect( @post.read_by?(@john)   ).to be_false
      expect( @post.read_by?(@paul)   ).to be_false
      expect( @post.read_by?(@george) ).to be_false
      expect( @post.read_by?(@ringo)  ).to be_true
    end
  end

  describe '#get_full' do
    before do
      members = (1..5).map do
        Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
      end

      @member = members.first
      @post = Libertree::Model::Post.create(FactoryGirl.attributes_for( :post, member_id: members.sample.id, text: 'post' ))
      5.times do |i|
        comment = Libertree::Model::Comment.create(FactoryGirl.attributes_for(:comment, member_id: members.sample.id, post_id: @post.id))
        4.times do |i|
          Libertree::Model::CommentLike.create(FactoryGirl.attributes_for(:comment_like, member_id: members.sample.id, comment_id: comment.id))
        end
      end
      3.times do
        Libertree::Model::PostLike.create(FactoryGirl.attributes_for(:comment_like, member_id: members.sample.id, post_id: @post.id))
      end
    end

    it 'gets the post with all comments, likes, and members' do
      p = Libertree::Model::Post.get_full(@post.id)

      # expect none of the following methods to hit the database
      expect( Libertree::Model::Comment     ).not_to receive(:on_post)
      expect( Libertree::Model::CommentLike ).not_to receive(:where)
      expect( Libertree::Model::PostLike    ).not_to receive(:where)

      expect( p.id ).to eq(@post.id)
      expect( p.comments.count ).to eq(5)
      expect( p.comments.flat_map(&:likes).count ).to eq(20)
      expect( p.likes.count ).to eq(3)
    end

    it 'defines a comments method that accepts filter arguments' do
      p = Libertree::Model::Post.get_full(@post.id)

      expect( Libertree::Model::Comment ).not_to receive(:on_post)
      ids = p.comments.map(&:id).sort

      expect( p.comments(:from_id => ids.last).count ).to eq(1)
      expect( p.comments(:from_id => ids.last).map(&:id).first ).to eq(ids.last)

      expect( p.comments(:to_id => ids.last).count ).to eq(4)
      expect( p.comments(:to_id => ids.last).map(&:id) ).to eq(ids.take(4))

      expect( p.comments(:limit => 2).count ).to eq(2)
      expect( p.comments(:limit => 2).map(&:id) ).to eq(ids.last(2))
    end

    it 'defines a comments method that fetches fresh comments when passed :refresh_cache' do
      p = Libertree::Model::Post.get_full(@post.id)
      expect( p.comments.count ).to eq(5)
      comment = Libertree::Model::Comment.create(FactoryGirl.attributes_for(:comment, member_id: @member.id, post_id: p.id))
      expect( p.comments.count ).to eq(5)
      expect( p.comments(:refresh_cache => true).count ).to eq(6)
      expect( p.comments.count ).to eq(6)
    end
  end

  describe '.filter_by_query' do
    it 'filters multiple tags' do
      account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
      q = Libertree::Query.new("-#annoying -#boring", account.id)

      p1 = new_post "what an annoying day"
      p2 = new_post "I post #annoying cat photos"
      p3 = new_post "I am boring #boring"

      res = Libertree::Model::Post.filter_by_query(q.parsed, account).all
      expect( res ).to include(p1)
      expect( res ).not_to include(p2)
      expect( res ).not_to include(p3)
    end
  end

  describe '.urls_already_posted?' do
    context 'given some posts with URLs' do
      before do
        @member = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) ).member
        new_post "http://abc.com"
        new_post "http://abc.com/"
        new_post "http://abc.com/somepath"
        new_post "http://abc.com/somepath?q=test"
        new_post "http://abc.com/somepath?q=test&p=foo"
        new_post "lorem ipsum http://def.com/def"
        new_post "http://ghi.com/ghi lorem ipsum"
        new_post "lorem ipsum http://jkl.com/jkl lorem ipsum"
        new_post "lorem ipsum https://jkl.com/jkl lorem ipsum"
        new_post "lorem ipsum http://mno.net/mno lorem ipsum http://pqr.org/pqr lorem ipsum"
        new_post "lorem ipsum\n\nhttp://stu.info/stu\n\nlorem ipsum"
        new_post "lorem ipsum [lorem ipsum](http://vwx.biz/vwx) lorem ipsum"
        new_post "lorem ipsum\nlorem ipsum https://qed.net?text=%22To+be+or+not+to+be%3F%22%2C+that+is+the+question.%22\nlorem ipsum"
        new_post "aoeuhttp://yza.com/yza lorem ipsum"
        new_post "1 http://repeatedurl.com/abc"
        new_post "2 http://repeatedurl.com/abc"
        new_post "3 http://repeatedurl.com/abc"
        new_post "4 http://repeatedurl.com/abc"
      end

      it 'correctly reports the presence or absence of Posts which have those URLs' do
        [
          ['', '',],
          ['lorem ipsum ', '',],
          ['', ' lorem ipsum',],
          ['lorem ipsum ', ' lorem ipsum',],
        ].each do |prefix, suffix|
          expect(Libertree::Model::Post.urls_already_posted?("#{prefix}http://abc.co#{suffix}")).to be_nil
          expect(Libertree::Model::Post.urls_already_posted?("#{prefix}http://abc.com#{suffix}")).not_to be_nil
          expect(Libertree::Model::Post.urls_already_posted?("#{prefix}http://abc.com/#{suffix}")).not_to be_nil
          expect(Libertree::Model::Post.urls_already_posted?("#{prefix}http://abcxcom/#{suffix}")).to be_nil
          expect(Libertree::Model::Post.urls_already_posted?("#{prefix}http://abcxcom/x?#{suffix}")).to be_nil
          expect(Libertree::Model::Post.urls_already_posted?("#{prefix}http://abc.com/some#{suffix}")).to be_nil
          expect(Libertree::Model::Post.urls_already_posted?("#{prefix}http://abc.com/somepath#{suffix}")).not_to be_nil
          expect(Libertree::Model::Post.urls_already_posted?("#{prefix}http://abc.com/somepath?#{suffix}")).to be_nil
          expect(Libertree::Model::Post.urls_already_posted?("#{prefix}http://abc.com/somepath?q#{suffix}")).to be_nil
          expect(Libertree::Model::Post.urls_already_posted?("#{prefix}http://abc.com/somepath?q=#{suffix}")).to be_nil
          expect(Libertree::Model::Post.urls_already_posted?("#{prefix}http://abc.com/somepath?q=te#{suffix}")).to be_nil
          expect(Libertree::Model::Post.urls_already_posted?("#{prefix}http://abc.com/somepath?q=test#{suffix}")).not_to be_nil
          expect(Libertree::Model::Post.urls_already_posted?("#{prefix}http://abc.com/somepath?q=test&p=foo#{suffix}")).not_to be_nil
        end

        [
          'http://abc.com',
          'http://abc.com/',
          'http://abc.com/somepath',
          'http://abc.com/somepath?q=test',
          'http://abc.com/somepath?q=test&p=foo',
          'http://def.com/def',
          'http://ghi.com/ghi',
          'http://jkl.com/jkl',
          'https://jkl.com/jkl',
          'http://mno.net/mno',
          'http://pqr.org/pqr',
          'http://stu.info/stu',
          'http://vwx.biz/vwx',
          'https://qed.net?text=%22To+be+or+not+to+be%3F%22%2C+that+is+the+question.%22',
        ].each do |url|
          [
            ['', '',],
            ['lorem ipsum ', '',],
            ['', ' lorem ipsum',],
            ['lorem ipsum ', ' lorem ipsum',],
            ['lorem [ipsum](', ') lorem',],
          ].each do |prefix, suffix|
            expect(Libertree::Model::Post.urls_already_posted?("#{prefix}#{url[0..-3]}#{suffix}")).to be_nil
            expect(Libertree::Model::Post.urls_already_posted?("#{prefix}#{url}#{suffix}")).not_to be_nil, "expected to find an existing Post containing URLS in submitted text " + "#{prefix}#{url}#{suffix}".inspect
          end

          expect(Libertree::Model::Post.urls_already_posted?("aoeu#{url}")).to be_nil
          expect(Libertree::Model::Post.urls_already_posted?("#{url}otherchars")).to be_nil
          expect(Libertree::Model::Post.urls_already_posted?("#{url}/otherchars")).to be_nil
        end

        expect(Libertree::Model::Post.urls_already_posted?('http://yza.com/yza')).to be_nil
        expect(Libertree::Model::Post.urls_already_posted?('aoeuhttp://yza.com/yza')).to be_nil
        expect(Libertree::Model::Post.urls_already_posted?('aoeu http://yza.com/yza')).to be_nil
        expect(Libertree::Model::Post.urls_already_posted?('http://yza.com/yza aoeu')).to be_nil
        expect(Libertree::Model::Post.urls_already_posted?('aoeu http://yza.com/yza aoeu')).to be_nil

        expect(Libertree::Model::Post.urls_already_posted?('aoeu http://repeatedurl.com/abc aoeu')).to be_nil
      end
    end
  end
end
