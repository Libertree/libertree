# -*- coding: utf-8 -*-
require 'spec_helper'

describe Libertree::Model::ContactList do
  before do
    @account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
    @list = Libertree::Model::ContactList.create( FactoryGirl.attributes_for(:contact_list, account_id: @account.id) )
  end

  describe '#members' do
    it 'returns an array of members' do
      a = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
      b = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
      member1 = a.member
      member2 = b.member
      @list << member1
      @list << member2
      expect( @list.members ).to match_array [member1, member2]
    end
  end

  describe '#member_ids' do
    it 'returns an array of member ids' do
      a = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
      b = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
      member1 = a.member
      member2 = b.member
      @list << member1
      @list << member2
      expect( @list.member_ids ).to match_array [member1.id, member2.id]
    end

    it 'returns ids for the same members that #members returns' do
      a = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
      b = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
      member1 = a.member
      member2 = b.member
      @list << member1
      @list << member2
      expect( @list.member_ids ).to match_array @list.members.map(&:id)
    end
  end

  describe '#<<' do
    it 'will not add the same member more than once' do
      a = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
      member1 = a.member
      5.times { @list << member1 }
      expect( @list.member_ids ).to match_array [member1.id]
    end
  end
end
