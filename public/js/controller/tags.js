$(document).ready( function() {
  $('.tags #river-selector').select2({width: '450px'}).change( function (event) {
    event.preventDefault();
    var selector = $('#river-selector'),
        riverId = event.val,
        tags = selector.data('tags'),
        url = '/rivers/_add_term/'+riverId+'/'+tags;

    Libertree.UI.listHandler( selector, url );
  } );
} );
