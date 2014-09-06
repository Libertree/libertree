#!/usr/bin/env ruby

class GemfileMerger
  def initialize(dirs)
    @hash = dirs.reduce({}) do |gemfile, dir|
      contents = self.parse("../#{dir}/Gemfile")
      gemfile.merge!(contents) do |key,oldval,newval|
        (oldval + newval).uniq
      end
      gemfile
    end
  end

  def parse(filename)
    group = nil
    lines = IO.readlines(filename)
    lines.reduce({}) do |res, item|
      if m = item.match(/^group '(?<name>.*)' do/)
        group = m['name']
        res[group] = []
      elsif  m = item.match(/^end/)
        group = nil
      else
        # strip off any comments
        item = item.gsub(/(.+)#.*/, '\1').rstrip
        if group
          res[group] << item
        else
          res[:root] = []   if res[:root].nil?
          res[:root] << item
        end
      end

      res
    end
  end

  def print(except=["development"])
    @hash.each_pair do |key, stuff|
      if except.include? key
        next
      else
        if key == :root
          ignore = true
        else
          puts "group '#{key}' do"
        end
        puts stuff.join("\n")
        puts "end"  unless ignore
      end
    end
  end
end

# merge the Gemfiles of all sub-projects
gemfile = GemfileMerger.new ['libertree-model-rb', 'libertree-backend-rb', 'libertree-client-rb']
gemfile.print
