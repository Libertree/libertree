/*jslint white: true, indent: 2, todo: true */
/*global $, alert, Libertree */

Libertree.Pools = (function () {
  "use strict";

  return {
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

      var collect_link = $(this),
          post = collect_link.closest('div.post, div.post-excerpt'),
          postId = post.data('post-id');

      $('div.pools').remove();
      Libertree.UI.enableIconSpinner(collect_link.find('img'));

      $.get(
        '/pools/_index/' + postId,
        function(html) {
          Libertree.UI.disableIconSpinner(collect_link.find('img'));
          $(html)
            .insertAfter(post)
            .css({'position': 'fixed', 'max-height': '90%'})
            .modalBox();
        }
      );
      return false;
    },

    submit: function (event) {
      event.preventDefault();
      var submitButton = $(this),
          form = submitButton.closest('form'),
          window = submitButton.closest('div.pools.modal'),
          postId = window.data('post-id');

      submitButton.prop('disabled', true);
      Libertree.UI.addSpinner( form, 'append', 16 );

      $.post(form.attr('action'),
             form.serialize(),
             function (response) {
               var h = $.parseJSON(response),
                   post = $('.post[data-post-id="'+postId+'"], .post-excerpt[data-post-id="'+postId+'"]'),
                   link;

               if (h.success) {
                 window.modalBox('close');
                 if (+h.count > 0) {
                   post.find('a.collect').addClass('hidden');
                   post.find('a.collected').removeClass('hidden');
                 } else {
                   post.find('a.collect').removeClass('hidden');
                   post.find('a.collected').addClass('hidden');
                 }
                 Libertree.UI.fadingAlert(h.msg);
               } else {
                 alert(h.msg);
               }

               Libertree.UI.removeSpinner('div.pools.modal');
               submitButton.prop('disabled', false);
             });
      return;
    }
  };
}());
