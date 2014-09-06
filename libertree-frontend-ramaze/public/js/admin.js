$(document).ready( function() {
  $('.server.draggable').draggable( {
    revert: true
  } );
  $('.forest').droppable( {
    drop: function( event, ui ) {
      window.location = '/admin/forests/add/' + $(this).data('forest-id') + '/' + ui.draggable.data('server-id');
    }
  } );
  $('.main').droppable( {
    drop: function( event, ui ) {
      if( ui.draggable.data('forest-id') ) {
        window.location = '/admin/forests/ensure_absent/' + ui.draggable.data('forest-id') + '/' + ui.draggable.data('server-id');
      }
    }
  } );
  $(document).on('click', 'table.job td a.delete',
    function (event) {
      var $this = $(this),
          job = $this.closest('table.job'),
          jobId = job.data('job-id'),
          fn = function () {
            $.get(
              '/admin/jobs/destroy/' + jobId + '.json',
              function () {
                /* TODO: Check for success */
                job.fadeOut(300);
              }
            );
          };
      Libertree.UI.confirmAjax(event, $this.data('msg'), fn);
    });
} );
