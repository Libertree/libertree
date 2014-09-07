module Libertree
  class Query
    private
    class ParseError < StandardError; end

    def patterns
      {
        'phrase'       => /(?<sign>[+-])?"(?<arg>[^"]+)"/,
        'from'         => /(?<sign>[+-])?:from "(?<arg>.+?)"/,
        'river'        => /(?<sign>[+-])?:river "(?<arg>.+?)"/,
        'contact-list' => /(?<sign>[+-])?:contact-list "(?<arg>.+?)"/,
        'via'          => /(?<sign>[+-])?:via "(?<arg>.+?)"/,
        'visibility'   => /(?<sign>[+-])?:visibility (?<arg>[a-z-]+)/,
        'word-count'   => /(?<sign>[+-])?:word-count ?(?<arg>(?<comp>[<>]) ?(?<num>[0-9]+))/,
        'spring'       => /(?<sign>[+-])?:spring (?<arg>"(?<spring_name>.+?)" "(?<handle>.+?)")/,
        'flag'         => /(?<sign>[+-])?:(?<arg>forest|tree|unread|liked|commented|subscribed)/,
        'tag'          => /(?<sign>[+-])?#(?<arg>\S+)/,
        'word'         => /(?<sign>[+-])?(?<arg>\S+)/,
      }
    end

    def check_resource(res, term, &block)
      err = if res.is_a? Array
              res.any?(&:nil?)
            else
              res.nil?
            end
      if err
        if @fail_on_error
          fail ParseError, term
        end
      else
        yield(*res)
      end
    end

    # We need the river id only to prevent self-referential river
    # queries.  For general purpose queries this is not required.
    def initialize(query, account_id, river_id=nil, fail_on_error=false)
      @fail_on_error = fail_on_error
      @parsed_query = Hash.new
      @parsed_query.default_proc = proc do |hash,key|
        hash[key] = {
          :negations    => [],
          :requirements => [],
          :regular      => []
        }
      end

      scanner = StringScanner.new(query)
      until scanner.eos? do
        scanner.skip(/\s+/)
        patterns.each_pair do |key, pattern|
          if term = scanner.scan(pattern)
            match = term.match(pattern)
            group = case match[:sign]
                    when '+'
                      :requirements
                    when '-'
                      :negations
                    else
                      :regular
                    end

            case key
            when
              'phrase',
              'via',
              'visibility',
              'word-count',
              'flag',
              'tag'
              @parsed_query[key][group] << match[:arg]
            when 'from'
              # TODO: eventually remove with_display_name check
              member = (Model::Member.with_handle(match[:arg]) || Model::Member.with_display_name(match[:arg]))
              check_resource(member, term) do |member|
                @parsed_query[key][group] << member.id
              end
            when 'river'
              river = Model::River[label: match[:arg]]
              check_resource(river, term) do |river|
                @parsed_query[key][group] << river  if river_id && river.id != river_id
              end
            when 'contact-list'
              list = Model::ContactList[ account_id: account_id, name: match[:arg] ]
              check_resource(list, term) do |list|
                ids = list.member_ids
                @parsed_query[key][group] << [list.id, ids]  unless ids.empty?
              end
            when 'spring'
              # TODO: eventually remove with_display_name check
              member = (Model::Member.with_handle(match[:handle]) || Model::Member.with_display_name(match[:handle]))
              pool = Model::Pool[ member_id: member.id, name: match[:spring_name], sprung: true ]  if member
              check_resource([member, pool], term) do |list, pool|
                @parsed_query[key][group] << pool
              end
            when 'word'
              # Only treat a matched word as a simple word if it consists only of word
              # characters.  This excludes URLs or other terms with special characters.
              if match[:arg] =~ /^[[:word:]]+$/
                @parsed_query['word'][group] << match[:arg]
              else
                @parsed_query['phrase'][group] << match[:arg]
              end
            end

            # move on to the next term
            next @parsed_query
          end
        end
      end
      @parsed_query
    end

    public
    def parsed
      @parsed_query.dup
    end

    def simple
      tags = @parsed_query['tag'][:regular].map {|t| "##{t}"}
      rest = @parsed_query.
        select {|k| ['phrase', 'word'].include? k}.
        flat_map {|h| h.last[:regular]}
      (tags + rest).join(' ')
    end

    def to_s
      res = []
      apply_template = lambda do |template, groups|
        res += groups[:negations].map    {|v| '-' + template.call(v)}
        res += groups[:requirements].map {|v| '+' + template.call(v)}
        res += groups[:regular].map      {|v|       template.call(v)}
      end

      @parsed_query.each_pair do |key, groups|
        template = case key
                   when 'phrase'
                     lambda {|v| "\"%s\"" % v }
                   when 'via'
                     lambda {|v| ":via \"%s\"" % v }
                   when 'visibility'
                     lambda {|v| ":visibility \"%s\"" % v }
                   when 'word-count'
                     lambda {|v| ":word-count %s" % v }
                   when 'flag'
                     lambda {|v| ":%s" % v }
                   when 'word'
                     lambda {|v| v }
                   when 'tag'
                     lambda {|v| "#%s" % v }
                   when 'from'
                     lambda {|v| ":from %s" % Model::Member[v.to_i].handle }
                   when 'river'
                     lambda {|v| ":river %s" % Model::River[v.to_i].label }
                   when 'contact-list'
                     template = lambda {|v| Model::ContactList[v.first.to_i].label }
                   when 'spring'
                     lambda {|v| pool = Model::Pool[v.to_i]; ":spring \"%s\" \"%s\"" % [pool.name, pool.member.handle] }
                   end
        apply_template.call(template, groups)
      end
      res.join(' ')
    end
  end
end
