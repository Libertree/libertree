require 'spec_helper'

describe 'main', :type => :feature, :js => true do
  before :all do
    $skip_authentication = false
  end

  it 'requires successful authentication' do
    visit '/'
    page.should have_content('Username')
    page.should have_content('Password')
    page.should have_button('Login')
  end

  context 'when an account exists' do
    before :each do
      @account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
      @account.password = 'testpass'
      @account.save
    end

    it 'authenticates with good credentials' do
      visit '/login'
      fill_in 'username', :with => @account.username
      fill_in 'password', :with => 'testpass'
      click_on 'Login'

      page.should have_content('Test user logged in')
    end

    it 'rejects bad credentials' do
      visit '/login'
      fill_in 'username', :with => @account.username
      fill_in 'password', :with => 'wrongpassword'
      click_on 'Login'

      page.should have_no_content('by post time')
      page.should have_no_content('River:')
      page.should have_content('Invalid credentials')
      page.should have_button('Login')
    end
  end
end
