/*jslint white: true, indent: 2, todo: true */
/*global $, alert, Libertree */

Libertree.Pools = (function () {
  "use strict";

  var addPost = function(poolId, postId, post, x, y) {
      $.get(
        '/pools/add_post/' + poolId + '/' + postId,
        function(response) {
          var h = $.parseJSON(response);
          $('div.pools').remove();
          if(h.success) {
            post.find('a.collect').addClass('hidden');
            post.find('a.collected').removeClass('hidden');
            Libertree.UI.fadingAlert(h.msg, x, y);
          } else {
            alert(h.msg);
          }
        }
      );
    };

  return {
    createPoolAndAddPost: function(post) {
      var postId = post.data('post-id'),
          textField = post.find('.pools .chzn-search input'),
          poolName = textField.val();

      textField.prop('disabled', true);

      $.get(
        '/pools/create_pool_and_add_post/'+poolName+'/'+postId,
        function(response) {
          var h = $.parseJSON(response);
          if(h.success) {
            $('.pools.window').hide();
            post.find('a.collect').addClass('hidden');
            post.find('a.collected').removeClass('hidden');
          } else {
            alert(h.msg);
          }
          textField.prop('disabled', false);
        }
      );
    },

    removePostHandler: function(e) {
      e.preventDefault();
      var post = $(this).closest('div.post, div.post-excerpt'),
          postId = post.data('post-id'),
          poolId = $(this).data('pool-id');

      $.get(
        '/pools/_remove_post/' + poolId + '/' + postId,
        function() {
          /* TODO: Check for success */
          post.slideUp(1000);
        }
      );
      return false;
    },

    collectHandler: function(e) {
      e.preventDefault();
      if( $('.pools.window:visible').length ) {
        $('.pools.window').hide();
        return false;
      }

      var x = e.clientX,
          y = e.clientY,
          collect_link = $(this),
          post = collect_link.closest('div.post, div.post-excerpt'),
          postId = post.data('post-id');

      $('div.pools').remove();
      Libertree.UI.enableIconSpinner(collect_link.find('img'));

      $.get(
        '/pools/_index/' + postId,
        function(html) {
          Libertree.UI.disableIconSpinner(collect_link.find('img'));
          var o = $(html),
              option;

          o.insertAfter(post.find('.meta'));
          if( o.find('option').length === 2 ) {
            option = $('select#pool-selector option:last');
            addPost( option.val(), postId, post, x, y );
          } else {
            o.show();
            $('select#pool-selector').chosen( {
              //TRANSLATEME
              no_results_text: "<a href='#' class='create-pool-and-add-post'>Add to a new pool</a> called"
            } ).change( function() {
              addPost( $('select#pool-selector').val(), postId, post, e.pageX, e.pageY );
            } );
          }
          $('#pool_selector_chzn a.chzn-single.chzn-default').mousedown();
        }
      );
      return false;
    }

  };
}());
