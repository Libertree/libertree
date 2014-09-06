Libertree.Notifications = {
  setState: function (new_state) {
    var old_state = (new_state === 'seen') ? 'unseen' : 'seen';
    return function (event) {
      event.preventDefault();
      var ids = $(this).data('notification-ids'),
          target = $(event.target).attr('href');

      $(this).removeClass(old_state).addClass(new_state);
      /* Also toggle on Notifications page */
      $.each( ids, function(j, id) {
        $('.notification[data-notification-ids="['+id+']"]').removeClass(old_state).addClass(new_state);
      } );
      $.get('/notifications/'+new_state+'/' + ids.join('/'), function(data) {
        Libertree.Notifications.updateNumUnseen(data);
        /* resume link following when the GET request is complete */
        if (target) { window.location = target; }
      } );
    };
  },
  // n is of type string
  updateNumUnseen: function(n) {
    var title = document.title;
    if( n === '0' ) {
      $('#num-notifications-unseen').hide();
      title = title.replace( /^\([0-9]+\) /, '' );
    } else {
      $('#num-notifications-unseen').show();
      title = title.replace( /^\([0-9]+\)/, '('+n+')' );
      if( ! title.match(/^\([0-9]+\)/) ) {
        title = '('+n+') ' + title;
      }
    }
    document.title = title;
    $('#num-notifications-unseen').text(n);
  }
};
