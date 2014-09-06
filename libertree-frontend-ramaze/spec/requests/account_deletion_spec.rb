require 'spec_helper'

describe Controller::Accounts, :type => :feature, :js => true do
  include_context 'logged in'

  before :each do
    @account_id = @account.id
    @account_username = @account.username
  end

  context '#delete' do
    it 'describes the deletion requirements' do
      visit '/accounts/delete'
      page.should have_content('input your username below')
      page.should have_button('Confirm')
    end

    it 'does not delete the account when no username is entered' do
      Libertree::Model::Account[@account_id].should_not be_nil

      visit '/accounts/delete'

      page.should have_no_content('does not match your username')

      click_on 'Confirm'

      page.should have_content('does not match your username')
      Libertree::Model::Account[@account_id].should_not be_nil
    end

    it 'does not delete the account when something other than the username is entered' do
      Libertree::Model::Account[@account_id].should_not be_nil

      visit '/accounts/delete'

      page.should have_no_content('does not match your username')

      fill_in 'username', with: 'not the account username'
      click_on 'Confirm'

      page.should have_content('does not match your username')
      Libertree::Model::Account[@account_id].should_not be_nil
    end

    it "deletes the current account when the account's username is entered" do
      Libertree::Model::Account[@account_id].should_not be_nil

      visit '/accounts/delete'

      fill_in 'username', with: @account.username
      click_on 'Confirm'

      page.should have_content('Your account has been deleted.')
      page.should have_content('Username')
      page.should have_content('Password')
      page.should have_button('Login')

      Libertree::Model::Account[@account_id].should be_nil
      Libertree::Model::Account[username: @account_username].should be_nil
    end
  end
end
