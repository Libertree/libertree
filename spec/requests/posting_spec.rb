require 'spec_helper'

describe 'a local member', :type => :feature, :js => true do
  include_context 'logged in'

  context 'given an existing post' do
    before :each do
      @post = Libertree::Model::Post.create(
        FactoryGirl.attributes_for( :post, member_id: @account.member.id, text: 'A test post.' )
      )
    end

    it 'can see the post' do
      visit "/posts/show/#{@post.id}"

      page.should have_content('A test post.')
    end

    it 'can comment on the post' do
      @post.comments.count.should == 0

      visit "/posts/show/#{@post.id}"

      fill_in 'text', with: 'A test comment.'
      click_button 'Comment'

      page.should have_content('Comment successfully posted.')
      page.should have_no_content('A test comment.')
      Libertree::Model::Post[@post.id].comments.count.should == 1
    end
  end

  it 'can create a post' do
    visit '/posts/new'
    page.should have_content('New Post')

    fill_in 'text', with: 'Test post.'
    click_button 'Post'

    page.should have_content('Test post.')
    page.should have_content('seconds ago')
    page.should have_content(@account.username)
    page.should have_content('0 comments')

    page.should have_no_content('New Post')
    page.should have_no_button('Post')
  end
end
