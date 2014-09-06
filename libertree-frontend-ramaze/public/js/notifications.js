$(document).ready( function() {
  $('#menu-notifications').click( function() {
    if( $('#notifications-window').is(':visible') ) {
      Libertree.UI.hideWindows();
      return false;
    }

    if( $('#num-notifications-unseen').text() === '0' ) {
      window.location = '/notifications';
      return false;
    }

    Libertree.UI.hideWindows();
    $('#notifications-window').empty();
    Libertree.UI.addSpinner('#notifications-window', 'append');
    $('#notifications-window').
      load(
        '/notifications/_index',
        function(html) {
          Libertree.Session.ensureAlive(html);
          Libertree.UI.removeSpinner('#notifications-window');
          Libertree.Notifications.updateNumUnseen( $( $.trim(html) ).find('.n').text() );
        }
      ).toggle();
    return false;
  } );

  $(document).on('click', '.notification.seen a', function(e) {
    e.stopPropagation();
  } );

  $(document).on('click', '.notification.unseen', Libertree.Notifications.setState('seen') );
  $(document).on('click', '.notification.seen', Libertree.Notifications.setState('unseen') );

  $(document).on('click', '#mark-all-notifications-seen', function(event) {
    event.preventDefault();
    $.get('/notifications/seen/all', function () {
      $('.notification').removeClass('unseen').addClass('seen');
      Libertree.Notifications.updateNumUnseen('0');
    } );
  } );

  /* Cover up occasional inconsistency in backend code */
  Libertree.Notifications.updateNumUnseen( $('#num-notifications-unseen').text() );
} );
