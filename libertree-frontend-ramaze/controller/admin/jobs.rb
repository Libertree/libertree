module Controller
  module Admin
    class Jobs < Controller::Admin::Base
      map '/admin/jobs'

      before_all do
        if action.view_value.nil?
          require_admin
          init_locale
        end
        @view = 'admin'
      end

      provide(:json, type: 'application/json') { |action,value| value.to_json }

      def index(task=nil)
        @unfinished = Libertree::Model::Job.unfinished(task)
        @task = task
      end

      def retry(job_id)
        job = Libertree::Model::Job[ job_id ]
        if job
          job.retry!
        end
        redirect_referrer
      end

      def retry_all(task=nil)
        Libertree::Model::Job.unfinished(task).each do |job|
          job.retry!  if job
        end
        redirect_referrer
      end

      def destroy(job_id)
        job = Libertree::Model::Job[ job_id ]
        if job
          job.delete
        end
        if Ramaze::Current.action.wish == 'json'
          return { 'success' => true }
        else
          redirect_referrer
        end
      end

      def introduce
        if request.post?
          host = request['host'].to_s
          Libertree::Model::Job.create(
            {
              task: 'request:INTRODUCE',
              params: {
                'host' => host,
              }.to_json,
            }
          )
          flash[:notice] = _("INTRODUCE request pending for remote tree @ %s") % host
        end
        redirect_referrer
      end

    end
  end
end
