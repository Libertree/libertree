require 'spec_helper'

describe Libertree::Server::Responder::Helper do
  let(:subject_class) { Class.new }
  let(:subject) { subject_class.new }

  before :each do
    subject_class.class_eval { include Libertree::Server::Responder::Helper }
  end

  describe 'missing_parameters' do
    it 'returns required keys that are not in the params array' do
      given = { ten: 10, twenty: 20 }
      required = [ :ten, :twenty, :thirty ]
      subject.missing_parameters(given, required) == [ :thirty ]
    end
    it 'returns an empty array if all required keys are present' do
      given = { ten: 10, twenty: 20, thirty: 30 }
      required = [ :ten, :twenty, :thirty ]
      subject.missing_parameters(given, required) == [ ]
    end
    it 'returns an empty array if more than the required keys are present' do
      given = { ten: 10, twenty: 20, thirty: 30, fourty: 40 }
      required = [ :ten, :twenty, :thirty ]
      subject.missing_parameters(given, required) == [ ]
    end
    it 'complains about empty values' do
      given = { ten: 10, twenty: 20, thirty: "" }
      required = [ :ten, :twenty, :thirty ]
      subject.missing_parameters(given, required) == [ :thirty ]
    end
  end

  describe 'require_parameters' do
    it 'raises MissingParameter exception when a required parameter is missing' do
      given = { ten: 10, twenty: 20, thirty: "" }
      required = [ :ten, :twenty, :thirty ]

      expect { subject.require_parameters(given, *required) }.
        to raise_error(Libertree::Server::MissingParameter, 'thirty')
    end

    it 'returns nil when there are no missing parameters' do
      given = { ten: 10, twenty: 20, thirty: 30 }
      required = [ :ten, :twenty, :thirty ]

      subject.require_parameters(given, *required).should eq nil
    end
  end

  describe 'assert' do
    it 'raises NotFound with the given message when the first argument is nil' do
      expect { subject.assert(nil, "Oh dear!") }.
        to raise_error(Libertree::Server::NotFound, "Oh dear!")
    end

    it 'returns nil otherwise' do
      subject.assert("some object", "whatever").should eq nil
    end
  end
end
