# encoding: utf-8
require 'nokogiri'
require 'markdown'

module Libertree
  module Render
    Options = [ :filter_html,
                :smart,
                :strike,
                :autolink,
                :hard_wrap,
                :notes,
                :codeblock,
                :hashtags,
                :usernames,
                :spoilerblock
              ]

    def self.to_html_string(s, opts=Options)
      return ''  if s.nil? or s.empty?
      Markdown.new( s, *opts ).to_html.force_encoding('utf-8')
    end

    def self.to_html_nodeset(s, opts=Options)
      Nokogiri::HTML.fragment(self.to_html_string(s, opts))
    end
  end
end
