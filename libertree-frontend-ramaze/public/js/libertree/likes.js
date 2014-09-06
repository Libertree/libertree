Libertree.mkLike = function(type) {
  // TODO: merge the two update functions
  var update = (type === 'comment') ?
    function( entity, response ) {
      var num_likes = entity.find('.num-likes');
      num_likes.text( response['num_likes'] );
      num_likes.attr('title', response['liked_by']);
      num_likes.show();
    }
  :
    function( entity, response ) {
      var num_likes = entity.find('.num-likes').first();
      num_likes.find('.value').text( response['num_likes'] );
      num_likes.attr('title', response['liked_by']);
      num_likes.show();

      // mark post read
      if( entity.find('.mark-unread.hidden').length ) {
        entity.find('.mark-read').addClass('hidden');
        entity.find('.mark-unread').removeClass('hidden');
      }
    }
  ;

  return function(link, event, path) {
    event.preventDefault();
    var entity = link.closest(path);
    Libertree.UI.enableIconSpinner(link.find('img'));
    $.get(
      '/likes/'+type+'s/create/' + entity.data(type+'-id'),
      function(response) {
        var h = $.parseJSON(response);
        Libertree.UI.disableIconSpinner(link.find('img'));
        link.addClass('hidden');
        link.siblings('a.unlike').removeClass('hidden').data(type+'-like-id', h[type+'_like_id']);
        update( entity, h );
      }
    );
  };
};


Libertree.mkUnlike = function(type) {
  // TODO: merge these update functions
  var update = (type === 'comment') ?
    function( entity, response ) {
      var num_likes = entity.find('.num-likes');
      num_likes.text( response['text'] );
      if( response['num_likes'] === 0 ) {
        num_likes.hide();
      } else {
        num_likes.attr('title', response['liked_by']);
      }
    }
  :
    function( entity, response ) {
      var num_likes = entity.find('.num-likes').first();
      num_likes.find('.value').text( response['num_likes'] );
      num_likes.attr('title', response['liked_by']);
    }
  ;

  return function(link, event, path) {
    event.preventDefault();
    var entity = link.closest(path);
    Libertree.UI.enableIconSpinner(link.find('img'));
    $.get(
      '/likes/'+type+'s/destroy/' + link.data(type + '-like-id'),
      function(response) {
        var h = $.parseJSON(response);
        Libertree.UI.disableIconSpinner(link.find('img'));
        link.addClass('hidden');
        link.siblings('a.like').removeClass('hidden');
        update( entity, h );
      }
    );
  };
};
