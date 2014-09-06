$(document).ready( function() {
  $('.profile #contact-list-selector').select2({width: '450px'}).change( function (event) {
    event.preventDefault();
    var selector = $('#contact-list-selector'),
        contactListId = event.val,
        memberId = selector.data('member-id'),
        url = '/contact-lists/add_member/'+contactListId+'/'+memberId;

    Libertree.UI.listHandler( selector, url );
  } );
} );
