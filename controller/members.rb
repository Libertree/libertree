module Controller
  class Members < Base
    map '/members'

    before_all do
      default_before_filter
    end

    layout do |path|
      case path
      when 'heartbeat'
        nil
      else
        :default
      end
    end

    provide(:json, type: 'application/json') { |action,value| value.to_json }

    def member_to_autocompletion_hash(member)
      if member.name_display == member.handle
        selection_text = member.handle
      else
        selection_text = "#{member.name_display} (#{member.handle})"
      end

      {
        'label' => selection_text,
        'value' => member.handle,
        'avatar_img_src' => member_img_path(member),
      }
    end

    def autocomplete_handle
      query = request['q'].to_s.strip
      return []  if query.empty?

      members = Libertree::Model::Member.search(query)
      commenting_members = []

      post = Libertree::Model::Post[ request['commenters_of_post_id'].to_i ]
      if post
        commenting_members = post.comments.map(&:member) & members
      end

      {
        'commenting_members' => commenting_members.map { |m|
          member_to_autocompletion_hash(m)
        }.sort_by { |hash|
          hash['label'].downcase
        },
        'members' => (members - commenting_members).map { |m|
          member_to_autocompletion_hash(m)
        }.sort_by { |hash|
          hash['label'].downcase
        },
      }
    end
  end
end
