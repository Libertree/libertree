require 'spec_helper'

describe Libertree::XML::Parser do
  it 'calls handle_stanza on the initialisation argument' do
    caller = mock("Caller")
    parser = Libertree::XML::Parser.new(caller)

    caller.should_receive(:handle_stanza)
    parser.receive_data "<iq/>"
  end

  it 'parses nested tags fine' do
    stanza_string = %{<iq type="set" id="12345">
  <some>
    <nested>stuff</nested>
    <more>stuff</more>
  </some>
</iq>}

    caller = mock("Caller")
    caller.stub(:handle_stanza) {|stanza| expect(stanza.to_s).to eq(stanza_string) }

    parser = Libertree::XML::Parser.new(caller)
    parser.receive_data stanza_string
  end
end
