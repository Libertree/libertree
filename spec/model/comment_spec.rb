require 'spec_helper'

describe Libertree::Model::Comment do
  before do
    @account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
    @member = @account.member
    @post = Libertree::Model::Post.create(
      FactoryGirl.attributes_for( :post, member_id: @member.id, text: 'Example post.' )
    )
  end

  def new_comment(text)
    @comment = Libertree::Model::Comment.create(
      FactoryGirl.attributes_for( :comment, member_id: @member.id, post_id: @post.id, text: text )
    )
  end

  describe '#glimpse' do
    context 'when the text is short' do
      before :each do
        new_comment 'Short text.'
      end
      it 'is all the text' do
        @comment.glimpse.should == 'Short text.'
      end
    end

    context 'when the text is long' do
      before :each do
        new_comment 'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec a diam lectus. Sed sit amet ipsum mauris. Maecenas congue ligula ac quam viverra nec consectetur ante hendrerit.'
      end
      it 'is only some of the beginning of the text' do
        @comment.glimpse.should == 'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Don...'
      end
    end

    context 'when the text has a quotation' do
      before :each do
        new_comment "> Here is a quote.\n\nHere is a reply."
      end
      it 'the quotation is not included' do
        @comment.glimpse.should == 'Here is a reply.'
      end
    end
  end

  describe '.search' do
    before :each do
      Libertree::DB.dbh.execute 'TRUNCATE comments CASCADE'
      @p1 = new_comment 'This is a test comment.'
      @p2 = new_comment 'This is also a test comment.'
      @p3 = new_comment 'This is a comment.'
      @p4 = new_comment 'That is a comment.'
      @p5 = new_comment 'me and my hat'
      @p6 = new_comment "'Twas brillig, and the slithy toves Did gyre and gimble in the wabe"
      @p7 = new_comment 'This was not good.  However, that is.'
    end

    # Expect the given query to return the given Array of Comments (in any order).
    def expect_search_match(query, comments)
      results = Libertree::Model::Comment.search(query)
      expect(comments - results).to be_empty, "Expected #{query.inspect} to also match #{(comments-results).map(&:text)}"
      expect(results - comments).to be_empty, "Expected #{query.inspect} not to match #{(results-comments).map(&:text)}"
    end

    it 'matches comments to single word queries' do
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

    it 'matches comments to multi-word queries' do
      expect_search_match 'This is', [ @p1, @p2, @p3, @p7 ]
      expect_search_match 'is this', [ @p1, @p2, @p3, @p7 ]
      expect_search_match 'that is', [ @p4, @p7 ]
      expect_search_match 'comment this', [ @p1, @p2, @p3 ]
    end
  end
end
