/*jslint white: true, indent: 2, todo: true */
/*global $, Libertree */

$(document).ready( function() {
  "use strict";

  var scrollable = Libertree.UI.scrollable();

  $(document).on('click', '.excerpts-view #post-new input[type="submit"]', Libertree.Posts.create);

  $(document).on('keydown', '#textarea-post-new', function(event) {
    if( $('#post-new .message').is(':visible') ) {
      $('#post-new .message').slideUp();
    }
  } );

  $(document).on('click', '.post-excerpt .show-more', function (event) {
    event.preventDefault();
    Libertree.UI.showMore($(this));
  });
  $(document).on('click', '.post-excerpt .show-less', function (event) {
    event.preventDefault();
    Libertree.UI.showLess($(this));
  });

  scrollable.mousewheel( function(event, delta, deltaX, deltaY) {
    scrollable.stop();
  } );

  $(document).on('click', '.post-excerpt .post-tools a.comment', function (event) {
    event.preventDefault();
    Libertree.UI.jumpToCommentField( $(this).closest('.post-excerpt') );
  });

  /* Displays "show more" when hovering over an image.
     This is necessary for two reasons:
     - when the image has not been loaded yet, the post contents
       might not immediately overflow.
     - per account setting, images may be displayed as thumbnails,
       only overflowing the container on hover
  */
  $(document).on('mouseover', '.overflowed img', function() {
    // do not do anything when this post is currently being expanded
    var excerpt = $(this).closest('.excerpt'),
        overflowed = excerpt.find('.overflowed').not(':animated');

    // NOTE: we cannot use Libertree.UI.showShowMores() because that would inspect *all* excerpts
    if( overflowed.length > 0 && excerpt.find('.post-text').height() > overflowed.height() ) {
      excerpt.siblings('.show-more').show();
    }
  } );

  $(document).on('click', '.load-more', function(event) {
    event.preventDefault();

    $('#no-more-posts').remove();
    $('.more-posts-divider').removeClass('more-posts-divider');
    $('.post-excerpt:first').addClass('more-posts-divider');

    Libertree.UI.addSpinner($(this).parent(), 'append');
    Libertree.PostLoader.loadFromRiver(
      $('#post-excerpts').data('river-id'),
      'newer',
      $('.post-excerpt:first').data('t'),
      function() {
        $('.more-posts').hide().detach().prependTo('#post-excerpts');
        $('.more-posts .n').text('0');
      }
    );
  } );

  /* ---------------------------------------------------- */

  Libertree.UI.showShowMores();
  Libertree.UI.initSpoilers();
} );
