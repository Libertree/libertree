require 'libertree/client'
require 'libertree/model'
require 'net/http'
require 'uri'

class JobProcessor
  def initialize(config_filename)
    @config_filename = config_filename
    @conf = YAML.load( File.read(config_filename) )
    if @conf['log_path']
      @log = File.open( @conf['log_path'], 'a+' )
      @log.sync = true
    else
      @log = $stdout
    end
    @pid = Process.pid
    @log_identifier = "jobp #{@pid}"

    if @log.respond_to? :path
      puts "pid #{@pid} logging to #{File.absolute_path(@log.path)}"
    end
  end

  def log(s, level = nil)
    t = Time.now.strftime("%Y-%m-%d %H:%M:%S")
    if level
      l = "#{level} "
    end

    @log.puts "[#{t}] (#{@log_identifier}) #{l}#{s}"
  end

  def log_error(s)
    log s, 'ERROR'
  end

  # @return [Job] nil if no job was reserved
  def reserve
    job = Libertree::Model::Job.s1("SELECT * FROM jobs WHERE pid IS NULL AND tries < 5 AND time_to_start <= NOW() ORDER BY time_to_start ASC LIMIT 1")
    return  if job.nil?

    Libertree::Model::Job.update(
      {
        id: job.id,
        pid: nil,
      },
      {
        pid: Process.pid,
        time_started: Time.now
      }
    )

    job = Libertree::Model::Job[job.id]
    if job.pid == Process.pid
      job
    end
  end

  def run
    quit = false

    terminate = Proc.new {
      quit = true
      puts "Quitting."
    }
    Signal.trap("TERM", &terminate)
    Signal.trap("INT" , &terminate)
    Signal.trap("HUP") do
      puts "\nReloading configuration."
      @log.close
      self.send(:initialize, @config_filename)
    end

    until quit
      job = reserve
      if job
        process job
      end
      3.times { sleep 1 unless quit }
    end
  end

  def process(job)
    log "Processing: job #{job.id}"

    # TODO: Maybe this code is too defensive, checking for nil comment, like post, etc.
    # Removing the checks would clean up the code a bit.
    case job.task
    when 'request:COMMENT'
      comment = Libertree::Model::Comment[job.params['comment_id'].to_i]
      retry_later = false
      if comment
        with_tree(job.params['server_id']) do |tree|
          response = tree.req_comment(comment)
          if response['code'] == 'NOT FOUND'
            # Remote didn't recognize the comment author or the referenced post
            # Send the potentially missing data, then retry the comment later.
            retry_later = true
            case response['message']
            when /post/
              if comment.post.local?
                tree.req_post comment.post
              end
            when /member/
              tree.req_member comment.member
            else
              if comment.post.local?
                tree.req_post comment.post
              end
              tree.req_member comment.member
            end
          end
        end
      end

      if ! retry_later
        job.time_finished = Time.now
      end
    when 'request:COMMENT-DELETE'
      with_tree(job.params['server_id']) do |tree|
        tree.req_comment_delete job.params['comment_id']
      end
      job.time_finished = Time.now
    when 'request:COMMENT-LIKE'
      like = Libertree::Model::CommentLike[job.params['comment_like_id'].to_i]
      if like
        with_tree(job.params['server_id']) do |tree|
          tree.req_comment_like like
        end
      end
      job.time_finished = Time.now
    when 'request:COMMENT-LIKE-DELETE'
      with_tree(job.params['server_id']) do |tree|
        tree.req_comment_like_delete job.params['comment_like_id']
      end
      job.time_finished = Time.now
    when 'request:FOREST'
      forest = Libertree::Model::Forest[job.params['forest_id'].to_i]
      with_tree(job.params['server_id']) do |tree|
        tree.req_forest forest
      end
      job.time_finished = Time.now
    when 'request:MEMBER'
      member = Libertree::Model::Member[job.params['member_id'].to_i]
      if member
        with_tree(job.params['server_id']) do |tree|
          tree.req_member member
        end
      end
      job.time_finished = Time.now
    when 'request:POST'
      post = Libertree::Model::Post[job.params['post_id'].to_i]
      if post
        with_tree(job.params['server_id']) do |tree|
          tree.req_post post
        end
      end
      job.time_finished = Time.now
    when 'request:POST-DELETE'
      with_tree(job.params['server_id']) do |tree|
        tree.req_post_delete job.params['post_id']
      end
      job.time_finished = Time.now
    when 'request:POST-LIKE'
      like = Libertree::Model::PostLike[job.params['post_like_id'].to_i]
      if like
        with_tree(job.params['server_id']) do |tree|
          tree.req_post_like like
        end
      end
      job.time_finished = Time.now
    when 'request:POST-LIKE-DELETE'
      with_tree(job.params['server_id']) do |tree|
        tree.req_post_like_delete job.params['post_like_id']
      end
      job.time_finished = Time.now
    when 'http:avatar'
      member = Libertree::Model::Member[ job.params['member_id'] ]

      begin
        uri = URI.parse(job.params['avatar_url'])
        if uri.path.empty?
          log_error "URL contains no path: #{job.params['avatar_url']}"
        else
          Timeout.timeout(10) do
            Net::HTTP.start(uri.host, uri.port) { |http|
              resp = http.get(uri.path)
              ext = File.extname(uri.path)
              if ! ['.png', '.gif', '.jpg', '.jpeg'].include?(ext.downcase)
                log_error "Invalid avatar file type: #{ext}"
                # TODO: mark this job as failed
              else
                File.open( "#{@conf['avatar_dir']}/#{member.id}#{ext}", 'wb' ) { |file|
                  file.write(resp.body)
                }
                member.avatar_path = "/images/avatars/#{member.id}#{ext}"
                job.time_finished = Time.now
              end
            }
          end
        end
      rescue URI::InvalidURIError, ArgumentError => e
        # TODO: mark this job as failed, because the URL cannot be parsed
        log_error "Invalid URI: #{job.params['avatar_url']}"
      rescue Timeout::Error
        # ignore
      end
    end

    if job.time_finished.nil?
      # Return job to queue.
      job.set(
        time_started: nil,
        pid: nil,
        tries: job.tries + 1,
        time_to_start: Time.now + 60*5
      )
    end

    log "Leaving: job #{job.id}"
  rescue Exception => e
    log_error "Error processing job #{job.id}: #{e.class} #{e.message}\n" + e.backtrace.join("\n\t")
  end

  def lt_client(remote_host)
    key = OpenSSL::PKey::RSA.new File.read(@conf['private_key_path'])
    c = Libertree::Client.new(
      public_key: key.public_key,
      private_key: key,
      avatar_url_base: @conf['avatar_url_base'],
      server_ip: @conf['ip_public'],
      server_name: @conf['server_name'],
      log: @log,
      log_identifier: @log_identifier
    )

    if c
      c.connect remote_host
      if block_given?
        yield c
        c.close
      end
    end

    c
  end

  def with_tree(server_id)
    server = Libertree::Model::Server[server_id]
    if server.nil?
      log_error "No server with id #{server_id.inspect}"
    else
      begin
        lt_client(server.ip) do |client|
          yield client
        end
      rescue Errno::ETIMEDOUT, Errno::ECONNREFUSED => e
        log_error "With #{tree.name_display} (#{tree.ip}): #{e.message}"
      end
    end
  end
end

if ARGV[0].nil?
  $stderr.puts "#{$0} <config.yaml>"
  exit 1
end
JobProcessor.new( ARGV[0] ).run
