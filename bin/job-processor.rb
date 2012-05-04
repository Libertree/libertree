require 'libertree/client'
require 'libertree/model'

class JobProcessor
  def initialize(config_filename)
    @conf = YAML.load( File.read(config_filename) )
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
    while 1+1 == 2
      job = reserve
      if job
        process job
      end
      sleep 3
    end
  end

  def process(job)
    puts "Processing: job #{job.id}"

    case job.task
    when 'request:COMMENT'
      comment = Libertree::Model::Comment[job.params['comment_id'].to_i]
      if comment
        with_forest do |tree|
          tree.req_comment comment
        end
      end
      job.time_finished = Time.now
    when 'request:COMMENT-DELETE'
      with_forest do |tree|
        tree.req_comment_delete job.params['comment_id']
      end
      job.time_finished = Time.now
    when 'request:COMMENT-LIKE'
      like = Libertree::Model::CommentLike[job.params['comment_like_id'].to_i]
      if like
        with_forest do |tree|
          tree.req_comment_like like
        end
      end
      job.time_finished = Time.now
    when 'request:COMMENT-LIKE-DELETE'
      with_forest do |tree|
        tree.req_comment_like_delete job.params['comment_like_id']
      end
      job.time_finished = Time.now
    when 'request:MEMBER'
      member = Libertree::Model::Member[job.params['member_id'].to_i]
      if member
        with_forest do |tree|
          tree.req_member member
        end
      end
      job.time_finished = Time.now
    when 'request:POST'
      post = Libertree::Model::Post[job.params['post_id'].to_i]
      if post
        with_forest do |tree|
          tree.req_post post
        end
      end
      job.time_finished = Time.now
    when 'request:POST-DELETE'
      with_forest do |tree|
        tree.req_post_delete job.params['post_id']
      end
      job.time_finished = Time.now
    when 'request:POST-LIKE'
      like = Libertree::Model::PostLike[job.params['post_like_id'].to_i]
      if like
        with_forest do |tree|
          tree.req_post_like like
        end
      end
      job.time_finished = Time.now
    when 'request:POST-LIKE-DELETE'
      with_forest do |tree|
        tree.req_post_like_delete job.params['post_like_id']
      end
      job.time_finished = Time.now
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

    puts "Leaving: job #{job.id}"
  rescue Exception => e
    $stderr.puts "Error processing job #{job.id}: #{e.message}\n" + e.backtrace.join("\n\t")
  end

  def lt_client(remote_host)
    c = Libertree::Client.new(
      public_key: File.read(@conf['public_key_path']),
      private_key: File.read(@conf['private_key_path']),
      avatar_url_base: @conf['avatar_url_base'],
      server_name: @conf['server_name']
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

  def with_forest
    @conf['forest'].each do |host|
      lt_client(host) do |client|
        yield client
      end
    end
  end
end

if ARGV[0].nil?
  $stderr.puts "#{$0} <config.yaml>"
  exit 1
end
JobProcessor.new( ARGV[0] ).run
