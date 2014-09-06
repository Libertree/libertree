$(document).ready( function() {
  $(document).on('click', '#textarea-message-new input[type="submit"]', Libertree.UI.TextAreaBackup.disable);

  $('#toggle-new-message, a.reply, a.reply-to-all').click( function() {
    $('form#new-message').slideDown();
    return false;
  } );

  $('a.reply').click( function (event) {
    event.preventDefault();
    $('input#recipients').val('sender').trigger('change');
  } );

  $('a.reply-to-all').click( function (event) {
    event.preventDefault();
    $('input#recipients').val('all').trigger('change');
  } );

  $(document).on('click', '#message-list .delete',
    function (event) {
      var $this = $(this),
          message = $this.closest('div.message'),
          messageId = message.data('message-id'),
          fn = function () {
            $.get(
              '/messages/delete/' + messageId + '.json',
              function () {
                /* TODO: Check for success */
                message.slideUp(1000);
              }
            );
          };
      Libertree.UI.confirmAjax(event, $this.data('msg'), fn);
    });

  $('input#recipients').select2(
      jQuery.extend(
          Libertree.UI.selectDefaults,
          {
            initSelection: function (element, callback) {
              // this is needed to auto-fill the input when replying
              var data = JSON.parse($('input#participants').val());
              // only pick first unless all are specified
              if (element.val() !== 'all') {
                  data = [ data[0] ];
              }
              element.val('');
              callback(data);
            }
          }));
} );
