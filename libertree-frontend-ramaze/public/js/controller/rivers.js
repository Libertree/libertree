$(document).ready( function() {
  var independentLabel = false;

  function copyQueryToLabel() {
    if( ! independentLabel ) {
      $('.form-river input[name="label"]').val( $('.form-river textarea[name="query"]').val() );
    }
  };

  $('.form-river textarea[name="query"]').focus();
  if( $('.form-river textarea[name="query"]').val() === $('.form-river input[name="label"]').val() ) {
    $('.form-river textarea[name="query"]').change( copyQueryToLabel );
    $('.form-river textarea[name="query"]').keyup( copyQueryToLabel );
  }

  $('.form-river input[name="label"]').change( function() {
    independentLabel = true;
  } );
} );
