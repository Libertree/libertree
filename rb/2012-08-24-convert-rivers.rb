require_relative 'lib/libertree/model'
include Libertree::Model

accounts = Account.s( %{
  SELECT *
  FROM accounts
  ORDER BY time_heartbeat DESC, id ASC
} )

accounts.each do |a|
  puts "-" * 79
  puts "Account #{a.id}\t#{a.username}"
  a.rivers.each do |r|
    puts "River #{r.id}\t#{r.label}"

    # This scan code was copy-pasted out of lib/libertree/model/river.rb
    # This is a one-shot script, so no need to be DRY.
    query_components = r.query.scan(/([+-]?"[^"]+")|([+-]?:from ".+?")|([+-]?:river ".+?")|(\S+)/).map { |c|
      c[3] || c[2] || c[1] || c[0].gsub(/^([+-])"/, "\\1").gsub(/^"|"$/, '')
    }
    new_query = query_components.map { |term|
      if term =~ /^:(tree|unread|liked|commented|subscribed)$/
        "+#{term}"
      else
        term
      end
    }.join(' ')

    puts "-\t#{r.query}"
    if new_query != r.query
      puts "+\t#{new_query}"

      if ARGV[0] == '--commit'
        begin
          r.revise(
            'label'           => r.label,
            'query'           => new_query,
            'appended_to_all' => r.appended_to_all
          )
        rescue StandardError => e
          puts e.class
          puts e.message
        end
      end
    end
  end
end
