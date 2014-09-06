/*jslint white: true, indent: 2, todo: true */
/*global $, Libertree, alert, confirm */

$(document).ready( function() {
  "use strict";

  $(document).on('click', '.jump-to-comment', function(event) {
    event.preventDefault();
    var commentsDiv = $(this).closest('div.comments'),
        commentsPane = $(this).closest('div.comments-pane'),
        targetScrollTop = commentsDiv.height() - commentsPane.height();

    commentsPane.animate(
      { scrollTop: targetScrollTop },
      targetScrollTop - commentsPane.scrollTop(),
      'easeOutQuint',
      function() {
        commentsPane.find('textarea').focus().hide().fadeIn();
      }
    );
  } );

  $(document).on('click', 'a.load-comments:not(.disabled)', function(event) {
    event.preventDefault();
    $(this).addClass('disabled');
    Libertree.Comments.loadMore($(this));
    return false;
  } );

  $(document).on('mouseover', 'div.comment', function() {
    $(this).find('.comment-tools').css('visibility', 'visible');
  } );
  $(document).on('mouseout', 'div.comment', function() {
    $(this).find('.comment-tools').css('visibility', 'hidden');
  } );

  $(document).on('click', '.comment .delete', function(event) {
    event.preventDefault();
    var $this = $(this),
        comment = $this.closest('.comment');

    if( confirm($this.data('msg')) ) {
      $.get( '/comments/destroy/' + comment.data('comment-id') );
      comment.fadeOut( function() { comment.remove } );
    }
  } );

  $(document).on('click', '.commenter-ref', function(event) {
    event.preventDefault();
    var source = $(this),
        member_id = source.data('member-id'),
        source_comment = source.closest('div.comment'),
        commentContainer = source.closest('div.comments'),
        candidates = commentContainer.find('div.comment[data-commenter-member-id="'+member_id+'"]').toArray().reverse(),
        target_comment = null;

    $.each( candidates, function() {
      if( Number($(this).data('comment-id')) < Number(source_comment.data('comment-id')) ) {
        target_comment = $(this);
        return false;
      }
    } );
    if( target_comment ) {
      var scrollable, targetScrollTop;

      scrollable = target_comment.closest('div.comments-pane');
      if( scrollable.length ) {
        targetScrollTop = scrollable.scrollTop() + target_comment.position().top;
      } else {
        scrollable = Libertree.UI.scrollable();
        targetScrollTop = target_comment.position().top;
      }

      scrollable.animate(
        { scrollTop: targetScrollTop },
        scrollable.scrollTop() - targetScrollTop,
        'easeOutQuint',
        function() {
          target_comment.find('.go-ref-back').attr('href', '#' + source_comment.attr('id')).show();
          target_comment.css('opacity', '0').animate({opacity: 1.0}, 2000);
        }
      );
    }
  } );
  $(document).on('click', '.go-ref-back', function () {
    $(this).hide();
  } );

  $(document).on('click', 'div.comment a.like', function(event) {
    Libertree.Comments.like( $(this), event, 'div.comment' );
  } );

  $(document).on('click', 'div.comment a.unlike', function(event) {
    Libertree.Comments.unlike( $(this), event, 'div.comment' );
  } );

  $(document).on('click', 'form.comment input.submit', Libertree.Comments.submit);

  $(document).on('click', '.detachable .detach', function(event) {
    event.preventDefault();
    var detachable = $(this).closest('.detachable'),
        offset = detachable.offset();

    detachable.addClass('detached');
    detachable.addClass('has-shadow');
    detachable.css('top', offset.top + 'px');
    detachable.css('left', offset.left + 'px');
    detachable.find('.detach').hide();
    detachable.find('.attach').show();
    detachable.draggable();
    return false;
  } );

  $(document).on('click', '.detachable .attach', function(event) {
    event.preventDefault();
    var detachable = $(this).closest('.detachable');
    detachable.removeClass('detached');
    detachable.removeClass('has-shadow');
    detachable.find('.attach').hide();
    detachable.find('.detach').show();
    detachable.draggable('destroy');
    return false;
  } );

  $(document).on('focus', 'textarea.comment', function() {
    $(this).addClass('focused');
  } );
  $(document).on('blur', 'textarea.comment', function() {
    $(this).removeClass('focused');
  } );

  /* ---------------------------------------------------- */

  if( window.location.hash.indexOf("#comment-") === 0 ) {
    Libertree.Comments.loadMore( $('a.load-comments'), true );
  }

  Libertree.Comments.hideLoadCommentsLinkIfAllShown( $('.post') );
} );
