require 'spec_helper'

describe Libertree::Embedder do

  describe '#extract_urls' do
    it 'strips off trailing punctuation' do
      text = "I watched a video (http://youtube.com/watch?v=123), it was boring."
      Libertree::Embedder.extract_urls(text).should == ["http://youtube.com/watch?v=123"]

      text = "I watched a video (http://youtube.com/watch?v=123)...."
      Libertree::Embedder.extract_urls(text).should == ["http://youtube.com/watch?v=123"]

      text = "I watched a video http://youtube.com/watch?v=123)...."
      Libertree::Embedder.extract_urls(text).should == ["http://youtube.com/watch?v=123"]

      text = "Silly. (I watched a video http://youtube.com/watch?v=123)...."
      Libertree::Embedder.extract_urls(text).should == ["http://youtube.com/watch?v=123"]

      text = "What do you think (http://youtube.com/watch?v=123)?"
      Libertree::Embedder.extract_urls(text).should == ["http://youtube.com/watch?v=123"]

      text = "(http://youtube.com/watch?v=123),..."
      Libertree::Embedder.extract_urls(text).should == ["http://youtube.com/watch?v=123"]

      text = "http://youtube.com/watch?v=123"
      Libertree::Embedder.extract_urls(text).should == ["http://youtube.com/watch?v=123"]

      text = "http://youtube.com/watch?v=123,"
      Libertree::Embedder.extract_urls(text).should == ["http://youtube.com/watch?v=123"]

      text = "http://youtube.com/watch?v=123."
      Libertree::Embedder.extract_urls(text).should == ["http://youtube.com/watch?v=123"]

      text = "http://youtube.com/watch?v=123.1"
      Libertree::Embedder.extract_urls(text).should == ["http://youtube.com/watch?v=123.1"]
    end

    it 'deals with URLs inside markdown links' do
      text = "Stupid [http://youtube.com/watch?v=123.1](http://youtube.com/watch?v=123.1)"
      Libertree::Embedder.extract_urls(text).should == ["http://youtube.com/watch?v=123.1"]
    end
  end
end
