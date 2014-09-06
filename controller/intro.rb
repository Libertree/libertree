# NOTE: be sure to paste the English tutorial to all languages that have no
# translation yet. Otherwise, meaningless keys like 'tutorial-step-1' are
# shown.

module Controller
  class Intro < Base
    map '/intro'
    before_all do
      default_before_filter
    end

    layout do |path|
      if path =~ /_post_icon/
        nil
      else
        :splash
      end
    end

    def index
      @view = "intro"
      @list_name = s_('intro-contact-list-name|Friends')
    end

  end
end
