module Controller
  module Admin
    class Forests < Controller::Admin::Base
      map '/admin/forests'

      before_all do
        if action.view_value.nil?
          require_admin
          init_locale
        end
      end

      layout :default

      # TODO:
      def _index
        @forests = Libertree::Model::Forest.all
      end

      def create
        redirect_referrer  if ! request.post?

        forest = Libertree::Model::Forest.create(
          name: request['name'].to_s,
          local_is_member: true
        )

        redirect_referrer
      end

      def add( forest_id, server_id )
        f = Libertree::Model::Forest[forest_id.to_i]
        if f.nil?
          redirect Admin::Main.r(:/)
        end

        if(
          f.local? && server_id == 'local' ||
          ! f.local? && server_id != 'local'
        )
          redirect Admin::Main.r(:/)
        end

        begin
          if ! f.local? && server_id == 'local'
            f.local_is_member = true
            f.save
          else
            s = Libertree::Model::Server[server_id.to_i]
            if s && f.local?
              f.add s
              Libertree::Model::Job.create_for_forests(
                {
                  task: 'request:FOREST',
                  params: { 'forest_id' => f.id, }
                },
                f
              )
            end
          end
        rescue Sequel::UniqueConstraintViolation => e
          if e.message =~ /violates unique constraint/
            flash[:error] = _('The tree is already a member of the forest.')
          else raise e end
        end

        redirect Admin::Main.r(:/)
      end

      def ensure_absent( forest_id, server_id )
        f = Libertree::Model::Forest[forest_id.to_i]
        if f.nil?
          redirect Admin::Main.r(:/)
        end

        if ! f.local?
          if server_id == 'local'
            f.local_is_member = false
            f.save
          end
        else
          s = Libertree::Model::Server[server_id.to_i]
          if s
            Libertree::Model::Job.create_for_forests(
              {
                task: 'request:FOREST',
                params: { 'forest_id' => f.id, }
              },
              f
            )
            f.remove s
          end
        end

        redirect Admin::Main.r(:/)
      end
    end
  end
end
