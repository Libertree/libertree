require 'spec_helper'

describe 'a local member', :type => :feature, :js => true do
  include_context 'logged in'

  def make_test_posts
    @posts = []
    @account2 = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
    5.times do |i|
      post = Libertree::Model::Post.create(
        FactoryGirl.attributes_for( :post, member_id: @account2.member.id, text: "Test post number #{i}." )
      )
      @posts << post
      # TODO: This should maybe be DRYed up with the code that runs in the job (backend)
      @account.rivers.each do |river|
        if river.should_contain? post
          river.add_post(post)
        end
      end
    end
  end

  context "when the member's account has no rivers" do
    context 'and there are no posts on the local server' do
      it 'sees an invitation to create a first river, and sees no posts' do
        visit '/home'

        page.should have_content('In order to see posts here, you need to have at least one river.')
        page.should have_link('Create one now')
        page.should have_no_content('ago')
      end
    end

    context 'and there are posts on the local server' do
      before :each do
        make_test_posts
      end

      it 'sees an invitation to create a first river, and sees no posts' do
        visit '/home'

        page.should have_content('In order to see posts here, you need to have at least one river.')
        page.should have_link('Create one now')
        page.should have_no_content('ago')
      end
    end
  end

  context 'given the member has a river' do
    before :each do
      Libertree::Model::River.create(
        FactoryGirl.attributes_for( :river, label: ':forest', query: ':forest', account_id: @account.id )
      )
    end

    context 'given some existing posts that match the river' do
      before :each do
        make_test_posts
      end

      it 'sees some of the posts' do
        visit '/home'
        page.should have_content('Test post number 4.')
        page.should have_content('Test post number 3.')
        page.should have_content('seconds ago')
      end
    end
  end
end
