/*jslint white: true, indent: 2 */
/*global $, Libertree */

$(document).ready( function() {
  "use strict";

  $(document).on('click', '.post-tools .collect, .post-tools .collected', Libertree.Pools.collectHandler);
  $(document).on('click', '.post-tools .remove', Libertree.Pools.removePostHandler);
  $(document).on('click', '.pools input.submit', Libertree.Pools.submit);

  $('.excerpts-view.pool #river-selector').select2({ width: '450px' }).change(function (event) {
    event.preventDefault();
    var selector = $('.excerpts-view.pool #river-selector'),
      riverId = event.val,
      poolId = selector.data('pool-id'),
      url = '/rivers/add_spring/'+riverId+'/'+poolId;

    Libertree.UI.listHandler( selector, url );
  } );
} );
