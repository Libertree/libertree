/*jslint white: true, indent: 2, todo: true */
/*global $, Libertree */

/* Taken from http://stackoverflow.com/a/14223324/28558 */
$.fn.textCursorPosition = function() {
  var pos;
  if (this[0].setSelectionRange) {
    pos = this[0].selectionStart;
  } else if (document.selection && document.selection.createRange) {
    var range = document.selection.createRange();
    pos = 0 - range.duplicate().moveStart('character', -100000);
  }
  return pos;
};

/* Taken from http://stackoverflow.com/a/10227475/28558 */
$.fn.setTextCursorPosition = function(index) {
  var range;

  /* different ways to do it due to browser differences */
  if (this[0].createTextRange) {
    range = this[0].createTextRange();
    range.move('character', index);
    range.select();
  } else {
    this[0].focus();
    if (this[0].selectionStart !== undefined) {
      this[0].setSelectionRange(index, index);
    }
  }
};

$.widget('custom.libertreeAutocomplete', $.ui.autocomplete, {
  _renderItem: function( ul, item ) {
    if (item.avatar_img_src) {
      return $('<li>')
        .append( $('<img src="'+item.avatar_img_src+'">') )
        .append( $('<a>').text(item.label) )
        .appendTo(ul);
    } else {
      return $('<li>').appendTo(ul);
    }
  }
});

Libertree.UI = (function () {
  "use strict";

  var newPostURLCheckTimeout,
    setSpeed = function(speed) {
      return function(pixels) {
        // calculate the duration to move an amount of pixels at a given speed
        return pixels * 1000 / speed;
      };
    },

    /* insert clickable prompt before hidden element to show it */
    initSpoiler = function (spoiler) {
      if( spoiler.prev('.spoiler-show').length === 0 ) {
        var msg = $('body').data('msg-spoiler-prompt'),
            link = $('<p class="spoiler-show"><a href="#">'+msg+'</a></p>');
        link.click(function (event) {
          event.preventDefault();
          spoiler.show();
          link.hide();
        });
        link.insertBefore(spoiler);
      }
    };

  return {
    // speed = pixels per second
    duration: setSpeed(600),
    threshold: 700, // pixels, same as used by @media queries in CSS
    autoResizeTextareas: true, // overridden by serverside initialization
    selectDefaults: {
      width: '450px',
      multiple: true,
      minimumInputLength: 3,
      //TRANSLATEME
      //formatSearching: function () { return "Searching ..." },
      formatInputTooShort: false, // do not show "type n more characters to search"
      initSelection : function (element, callback) {
        var data = [];
        $(element.val().split(",")).each(function () {
          var parts = this.split("=");
          data.push({id: parts[0], text: parts[1]});
        });
        element.val('');
        callback(data);
      },

      // TODO: move search.json out of messages to a shared location
      ajax: {
        url: "/messages/search.json",
        dataType: 'json',
        data: function (term, page) {
          return { q: term };
        },
        results: function (data, page) {
          return {results: data};
        }
      }
    },

    confirmAction: function (event) {
      if( ! confirm($(this).data('msg')) ) {
        event.preventDefault();
      }
    },

    confirmAjax: function(event, msg, fn) {
      event.preventDefault();
      if( confirm(msg) ) { fn(); }
    },

    listHandler: function (selector, url) {
      Libertree.UI.addSpinner( selector.parent(), 'append' );
      $.get( url,
        function() {
          Libertree.UI.removeSpinner( selector.parent() );
          Libertree.UI.fadingAlert( selector.data('msg') );
          selector.val('0');
          selector.trigger("liszt:updated");
        }
      );
    },

    memberHandleAutocompletion: function (event, ui) {
      if( ! ui.item ) { return; }

      var textfield = $(event.target),
        text = textfield.val(),
        indexOfAtSymbol = text.substring(0, textfield.textCursorPosition()).search(/@\S+$/),
        newText = text.substring(0, indexOfAtSymbol+1) + ui.item.value + text.substring(textfield.textCursorPosition());

      textfield.val(newText);
      textfield.setTextCursorPosition(indexOfAtSymbol + ui.item.value.length + 1);

      return false;
    },

    showShowMores: function(excerpts) {
      var set = excerpts;
      if (set == undefined || set == null) {
          set = $('.excerpt');
      }
      set.each( function() {
        if( $(this).find('.post-text').height() > $(this).find('.overflowed').height() ) {
          $(this).closest('.excerpt').siblings('.show-more').show();
        }
      } );
    },

    showMore: function (showMoreLink) {
      var excerpt = showMoreLink.siblings('.excerpt'),
          overflowed = excerpt.find('.overflowed'),
          excerptParent = showMoreLink.closest('.post-excerpt'),
          postId = excerptParent.data('post-id'),
          comments = excerptParent.find('div.comments'),
          commentHeight = comments.get(0).scrollHeight,
          heightDifference,
          animationDuration;

      Libertree.Posts.markRead(postId);
      showMoreLink.hide();

      //TODO: don't do this. Record the excerpt height somewhere and operate on that.
      overflowed.data( 'contracted-height', overflowed.height() );

      excerptParent.find('div.comments.hidden').removeClass('hidden');
      heightDifference = excerpt.get(0).scrollHeight - overflowed.height();
      animationDuration = Libertree.UI.duration(heightDifference);

      overflowed.animate(
        {
          height: excerpt.get(0).scrollHeight + 'px',
          'max-height': excerpt.get(0).scrollHeight + 'px'
        },
        animationDuration,
        function() {
          /* cancel explicit height set by animation */
          overflowed.height('auto');
          overflowed.css('max-height', 'none');
          showMoreLink.siblings('.show-less').show();
        }
      );

      return heightDifference;
    },

    showLess: function (link) {
      var excerpt = link.closest('.post-excerpt'),
          overflowed = excerpt.find('.overflowed'),
          comments = excerpt.find('div.comments'),
          distance = excerpt.height() - overflowed.data('contracted-height'),
          animationDuration = Libertree.UI.duration(distance),
          excerptTop = excerpt.position().top,
          scrollable = Libertree.UI.scrollable(),
          windowTop = scrollable.scrollTop(),
          scrollTop = excerptTop - windowTop;

      link.hide();

      if( scrollTop < 100 ){
        scrollable.animate(
          { scrollTop: windowTop + ( scrollTop - 100 ) },
          animationDuration
        );
      }

      overflowed.animate(
        { height: overflowed.data('contracted-height')+'px' },
        animationDuration,
        function() {
          /* set max-height, not height.  This makes for a smooth "show
           * more" animation in other themes. */
          overflowed.css('max-height', overflowed.data('contracted-height')+'px');
          overflowed.height('auto');
          $(this).closest('.post-excerpt').find('div.comments').addClass('hidden');
          link.siblings('.show-more').show();
        }
      );
    },

    initSpoilers: function () {
        $('div.spoilers').each( function () { initSpoiler($(this)); } );
    },

    jumpToCommentField: function (excerpt) {
      var scrollable = Libertree.UI.scrollable(),
          scrollTop = scrollable.scrollTop(),
          heightDifference,
          excerptTruncation,
          animationDuration;

      heightDifference = Libertree.UI.showMore(excerpt.find('.show-more'));
      animationDuration = Libertree.UI.duration(heightDifference);

      excerptTruncation = excerpt.position().top + excerpt.height() - scrollTop - $(window).height();
      if( excerptTruncation < 0 ) {
          excerptTruncation = 0;
      }

      scrollable.animate(
        { scrollTop: scrollTop + heightDifference + excerptTruncation },
        animationDuration,
        function() {
          if( ! Libertree.UI.isTouchInterface ) {
            excerpt.find('textarea.comment').focus();
          }
        }
      );
    },

    markdownInjector: function () {
      var $this = $(this),
          textarea = $this.closest('.markdown-injector').siblings('textarea');

      switch ($this.data('markdown')) {
      case "title":
        textarea.surroundSelectedText("\n## ", "");
        break;
      case "subtitle":
        textarea.surroundSelectedText("\n### ", "");
        break;
      case "bold":
        textarea.surroundSelectedText("**", "**");
        break;
      case "italic":
        textarea.surroundSelectedText("*", "*");
        break;
      case "strike":
        textarea.surroundSelectedText("~~", "~~");
        break;
      case "url":
        textarea.surroundSelectedText("[", "](URL)");
        break;
      case "image":
        textarea.surroundSelectedText("![IMAGE TITLE](", ")");
        break;
      case "image-link":
        textarea.surroundSelectedText("[![image/photo](", ")](URL)");
        break;
      case "quote":
        textarea.surroundSelectedText("\n\n> ", "");
        break;
      case "list":
        textarea.surroundSelectedText("\n\n* ", "");
        break;
      default:
        textarea.val( textarea.val() + $this.data('markdown') );
      }
      textarea.focus();
      return false;
    },

    hideWindows: function() {
      $('#chat-window.resizable').resizable('destroy');
      $('#chat-window').removeClass('resizable');
      $('.window').hide();
      Libertree.Chat.rememberDimensions();
    },

    //FIXME: src depends on selected theme
    addSpinner: function(target_selector, position, size) {
      //TODO: default value of "size"
      $(target_selector)[position]('<img class="spinner size-'+size+'" src="/themes/default/images/spinner.gif"/>');
    },

    removeSpinner: function(target_selector) {
      $('img.spinner', target_selector).remove();
    },

    //FIXME: src depends on selected theme
    enableIconSpinner: function(target) {
      target.data('src', $(target).attr('src'));
      target.attr('src', "/themes/default/images/icon-spinner.gif");
    },
    disableIconSpinner: function(target) {
      if ($(target).data('src') !== undefined) {
        target.attr('src', $(target).data('src'));
        target.removeAttr('data-src');
      }
    },

    //TRANSLATEME
    updateAges: function() {
      $('.age').each( function(i) {
        if( $(this).text().match(/^seconds ago$/) ) {
          $(this).text('1 minute ago');
        } else {
          var m = $(this).text().match(/^(\d+) minutes? ago$/);
          if( m ) {
            $(this).text( (parseInt(m[1], 10) + 1) + ' minutes ago');
          }
        }
      } );
    },

    // TODO: replace with bootstrap popover
    fadingAlert: function(message, x, y) {
      var div = $('<div class="fading-alert has-shadow">'+message+'</div>');
      div.appendTo('body');

      if( x !== undefined && y !== undefined ) {
        div.css( { left: x+'px', top: y+'px' } );
      }
      setTimeout(
        function() {
          $('.fading-alert').fadeOut(2000);
        },
        1000 + message.length * 50
      );
    },

    TextAreaBackup: (function () {
      var timer,
          stored = '';

      return {
        enable: function() {
          timer = setInterval(Libertree.UI.TextAreaBackup.save, 15 * 1000);
        },
        disable: function() {
          clearInterval(timer);
        },
        save: function() {
          $('textarea').each( function(i) {
            var text = $(this).val();
            if( text !== '' && text !== stored ) {
              stored = text;
              $.post(
                '/textarea_save',
                {
                  text: text,
                  id: $(this).attr('id')
                }
              );
              return false;
            }
          } );
        }
      };
    }()),

    isTouchInterface: (function() {
      return ("ontouchstart" in document.documentElement);
    }()),

    makeTextAreasExpandable: function() {
      if( ! Libertree.UI.autoResizeTextareas ) {
        return;
      }

      $('textarea').not('.textarea-chat').each( function() {
        /* TODO: Try to do something intelligent to dynamically determine a
        good number instead of hardcoded 60 */
        $(this).expandable( { maxRows: 60 } );
      } );
    },

    scrollable: function() {
      /* Chromium needs us to use body, Firefox and Opera need us to use html */
      if( $('html').scrollTop() > $('body').scrollTop() ) {
        return $('html');
      }
      return $('body');
    },

    animatableNodesOnly: function(nodes) {
      var array = $.grep( nodes, function(node, i) {
        /* Ignore text nodes and other types which cannot have jQuery animations (e.g. slideDown) called on them. */
        return node.nodeType === 1;
      } );
      return $(array);
    },

    indicateNewPosts: function (data) {
      var indicator = $('#post-excerpts[data-river-id="'+data.riverId+'"] .more-posts'),
          numNewPosts = data.postIds.length;

      if( indicator.length ) {
        /* Don't count posts which are already shown in the river */
        $.each( data.postIds, function(i, postId) {
          if( $('#post-'+postId).length ) {
            numNewPosts--;
          }
        } );
        if( numNewPosts > 0 ) {
          indicator.find('.load-more').text(data.newPostsMessage);
          indicator.slideDown();
        }
      }
    },

    renderPreview: function () {
      var $this = $(this),
          unrendered = $this.closest('form').find('textarea[name="text"]').val();

      // abort unless there is text to be rendered
      if (unrendered.length === 0) {
        return false;
      }

      var target = $this.closest('form.comment, form#post-new, form#post-edit, form#new-message'),
          preview_heading = $this.data('preview-heading'),
          close_label = $this.data('preview-close-label'),
          type = $this.data('type'),
          textType = null;

      if( type === 'post' ) {
        textType = 'post-text';
      }

      $.post(
        '/_render',
        { s: unrendered },
        function(html) {
          var scrollable = target.closest('div.comments-pane'),
              delta;

          Libertree.Session.ensureAlive(html);
          if( target.length > 0 ) {
            $('.preview-box').remove();
            target.append( $('<div class="preview-box" class="'+type+'"><a class="close-preview" href="#">'+close_label+'</a><h3 class="preview">'+preview_heading+'</h3><div class="text typed-text '+textType+'">' + html + '</div></div>') );
            Libertree.UI.initSpoilers();
            if( scrollable.length === 0 ) {
              scrollable = Libertree.UI.scrollable();
              delta = $('.preview-box').offset().top - scrollable.scrollTop() - 100;
            } else {
              delta = $('.preview-box').offset().top - 100;
            }
            scrollable.animate(
              { scrollTop: scrollable.scrollTop() + delta },
              delta * 2
            );
          }
        }
      );
    },

    // register content loaders as continuous scroll handlers
    registerScrollHandler: function () {
      var loaderContainer = $('.autoload-container'),
          loaderType,
          loaderArgs,
          loader;

      if (loaderContainer) {
        loaderType = loaderContainer.data('loader-type');
        loaderArgs = loaderContainer.data('loader-args');
        loader = Libertree.PostLoader.mkLoader(loaderType);

        $(window).scroll(function () {
          if( $(window).scrollTop() + $(window).innerHeight() >= $(document).height() - 600 ) {
            if( $('#no-more-posts').length ) { return; }
            loader(loaderArgs);
          }
        });
      }
    },

    restoreSidebar: function () {
      if (($.cookie('sidebar-status') == "true") && ! $('.excerpts-view #sidebar').is(":visible")) {
        Libertree.UI.toggleSidebar();
      }
    },

    toggleSidebar: function () {
      $('#sidebar').toggle();
      $('.excerpts-view #header').toggleClass('with-sidebar');
      $('.excerpts-view .panel').toggleClass('with-sidebar');
      $('#post-excerpts').toggleClass('with-sidebar');
      $('.excerpts-view #post-new').toggleClass('with-sidebar');

      if ($("body").hasClass("excerpts-view")) {
        $.cookie('sidebar-status', $('.excerpts-view #sidebar').is(":visible"), {'path': '/'});
      }
    },

    init: function() {
      $(document).ready( function () {
        Libertree.UI.registerScrollHandler();

        setInterval( Libertree.UI.updateAges, 60 * 1000 );
        Libertree.UI.TextAreaBackup.enable();
        Libertree.UI.makeTextAreasExpandable();
        if ($("body").hasClass("excerpts-view")) {
          Libertree.UI.restoreSidebar();
        }

        if( Libertree.UI.isTouchInterface ) {
          $('body').addClass('touch-interface');
        }
      });
    }
  };
}());

Libertree.UI.init();
