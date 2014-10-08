module Controller
  class Profiles < Base
    map '/profiles'
    before_all do
      default_before_filter
    end

    layout do |path|
      if path =~ %r{^_}
        nil
      else
        :default
      end
    end

    def show( member_id )
      @view = "excerpts-view profile"
      @member = Libertree::Model::Member[ member_id.to_i ]
      if @member.nil?
        redirect_referrer
      end

      @profile = @member.profile
      @posts = @member.posts
    end

    def _more( member_id, older_or_newer = 'older', time = Time.now.to_i )
      # TODO: this is called by the profile post loader.  Make sure that
      # only those posts are returned for which the requester has view permissions.
      member = Libertree::Model::Member[ member_id.to_i ]
      @posts = member.posts(
        limit: 8,
        time: time.to_f,
        newer: ( older_or_newer == 'newer' ),
      )
      render_file "#{Ramaze.options.views[0]}/posts/_excerpts.erb"
    end

    def edit
      @view = "profile edit"
      @profile = account.member.profile
      avatar_path = "/images/avatars/#{account.member.id}.png"
      @has_avatar = File.exists?(File.join(Ramaze.options.roots.first, Ramaze.options.publics.first, avatar_path))
    end

    def update
      return  if ! request.post?

      name_display = request['name_display'].to_s
      if name_display.strip.empty?
        name_display = nil
      end
      begin
        account.member.dirty
        account.member.profile.update(
          name_display: name_display,
          description: request['description'].to_s
        )
      rescue Sequel::CheckConstraintViolation => e
        if e.message =~ /valid_name_display/
          flash[:error] = _('Special characters are not allowed in display names.')
          redirect_referrer
        else
          raise e
        end
      end

      redirect r(:show, account.member.id)
    end

    def avatar_reset
      options = Controller::Accounts.options
      dir = File.join(options.roots.first, options.publics.first, 'images', 'avatars')
      basename = "#{account.member.id}.png"
      avatar_path = File.expand_path(basename, dir)

      begin
        FileUtils.rm avatar_path

        # This is required for distributing avatar deletion.  The
        # model library will send an empty avatar path to the forest.
        account.member.avatar_path = nil
        account.member.save

        flash[:notice] = _('Avatar deleted.')
      rescue
        flash[:error] = _('Failed to reset avatar.')
      end

      redirect_referrer
    end

    def avatar_upload
      return  if ! request.post?

      tempfile, filename, type = request['file'].values_at(:tempfile, :filename, :type)
      if type.split('/').first != 'image'
        flash[:error] = _('Only image files may be used as avatars.')
        redirect_referrer
      end
      extension = File.extname(filename).downcase
      if ! ['.png', '.jpg', '.jpeg', '.gif',].include?(extension)
        flash[:error] = _('Only .png, .jpeg and .gif files may be used as avatars.')
        redirect_referrer
      end

      options = Controller::Accounts.options
      dir = File.join(options.roots.first, options.publics.first, 'images', 'avatars')
      basename = "#{account.member.id}.png"
      save_path = File.expand_path(basename, dir)
      FileUtils.mkdir_p(dir)

      image = MiniMagick::Image.open(tempfile.path)
      image.combine_options do |c|
        c.thumbnail "40x40>"
        c.background "transparent"
        c.gravity "center"
        c.extent "40x40"
      end
      avatar_mask = File.join(options.roots.first, options.publics.first, 'images', 'avatar-mask.png')
      result = MiniMagick::Image.open(avatar_mask).composite(image) {|c| c.compose "In"}
      result.write save_path
      File.chmod  0644, save_path

      # This is required to distribute the avatar to the forest.
      # Distribution is handled by the frontend-agnostic model library
      # which makes no assumptions about the location of avatars.
      account.member.avatar_path = "/images/avatars/#{account.member.id}.png"
      account.member.save

      flash[:notice] = _('Avatar changed.')
      redirect_referrer
    end
  end
end
