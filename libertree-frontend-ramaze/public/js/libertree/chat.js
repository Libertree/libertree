/*jslint white: true, indent: 2 */
/*global $, Libertree, alert */

Libertree.Chat = (function () {
  "use strict";

  // n is of type string
  var updateNumUnseen = function(n) {
        if( n === '0' ) {
          $('#num-chat-unseen').hide();
        } else {
          $('#num-chat-unseen').show();
        }
        $('#num-chat-unseen').html(n);
      },

      // n is of type string
      updateNumUnseenForPartner = function(memberId, n) {
        var tab = $('#chat-window .tab[data-member-id="'+memberId+'"]'),
            indicator = tab.find('.num-chat-unseen');

        if( n === '0' ) {
          indicator.hide();
        } else {
          indicator.show();
        }
        indicator.html(n);
      },

      syncUIDimensions = function () {
        $('#chat-window .log.active .messages').height(
          $('#chat-window').height() - 200
        );
        $('#chat_new_partner_chzn').width(
          $('#chat-window').width() - 10
        );
      },

      activateConversation = function (memberId) {
        $('#chat-window .tab, #chat-window .log').removeClass('active');
        $('#chat-window .tab[data-member-id="'+memberId+'"]').addClass('active');
        $('#chat-window .log[data-member-id="'+memberId+'"]').addClass('active');
        $('#chat-window .log.active .textarea-chat').focus();
        syncUIDimensions();
        $('#chat-window .log.active .messages').scrollTop(999999);
      },

      fetchConversationWith = function (memberId, andActivate) {
        if( $('#chat-window .tab[data-member-id="'+memberId+'"]').length ) {
          activateConversation(memberId);
          return false;
        }

        $.get(
          '/chat/_tab/'+memberId,
          function(html) {
            $(html).appendTo('#chat-window .tabs');
          }
        );
        $.get(
          '/chat/_log/'+memberId,
          function(html) {
            var o = $(html);
            o.appendTo('#chat-window .logs');
            o.find('.messages').scrollTop(999999);
            o.find('.textarea-chat').focus();
            if( andActivate ) {
              activateConversation(memberId);
            }
          }
        );
      },

      markConversationSeen = function (memberId) {
        $.get(
          '/chat/seen/'+memberId,
          function(html) {
            updateNumUnseen(html);
            updateNumUnseenForPartner(memberId, '0');
          }
        );
      };

  return {
    fetchMessage: function(chatMessage) {
      var messages = $('#chat-window .log[data-member-id="'+chatMessage.partnerMemberId+'"] .messages');
      $.get(
        '/chat/_message/' + chatMessage.id,
        function(html) {
          var o = $( $.trim(html) ),
              height,
              animationDuration;

          o.appendTo(messages);
          height = o.height();
          animationDuration = height*5;

          o.hide().slideDown(animationDuration);

          messages.animate(
            { scrollTop: messages.scrollTop() + height + 30 },
            animationDuration
          );
        }
      );
    },

    receiveMessage: function(data) {
      var tab = $('#chat-window .tab[data-member-id="'+data.partnerMemberId+'"]');

      if( tab.length === 0 ) {
        fetchConversationWith(data.partnerMemberId, false);
      }

      if( $('#chat-window').is(':visible') && tab.hasClass('active') ) {
        markConversationSeen(data.partnerMemberId);
      } else {
        updateNumUnseen(data.numUnseen);
        updateNumUnseenForPartner(data.partnerMemberId, data.numUnseenForPartner);
      }

      Libertree.Chat.fetchMessage(data);
    },

    rememberDimensions: function() {
      // only remember position if this is not a device with a small screen
      if (document.documentElement.clientWidth > Libertree.UI.threshold) {
        $.cookie( 'chat-top', $('#chat-window').css('top') );
        $.cookie( 'chat-left', $('#chat-window').css('left') );
        $.cookie( 'chat-width', $('#chat-window').css('width') );
        $.cookie( 'chat-height', $('#chat-window').css('height') );
      }
      $.cookie( 'chat-open', $('#chat-window').is(':visible') );
    },

    heartbeat: function() {
      $.get('/accounts/heartbeat');
    },

    init: function () {
      $('#menu-chat').click( function() {
        if( $('#chat-window').is(':visible') ) {
          Libertree.UI.hideWindows();
          return false;
        }

        Libertree.UI.hideWindows();
        $('#chat-window').empty();
        Libertree.UI.addSpinner('#chat-window', 'append');
        $('#chat-window')
          .show()
          .load(
            '/chat/_index',
            function(html) {
              Libertree.Session.ensureAlive(html);
              Libertree.UI.removeSpinner('#chat-window');
              $('#chat-window').hide();
              var o = $( $.trim(html) ),
                  memberId = o.find('.log.active').data('member-id');

              if (memberId) {
                markConversationSeen( memberId );
              }

              $('#chat-window')
                .resizable( {
                  minHeight: 170,
                  resize: function(event, ui) {
                    syncUIDimensions();
                  },
                  stop: function(event, ui) {
                    Libertree.Chat.rememberDimensions();
                  }
                } )
              ;
              $('#chat-window').addClass('resizable');

              $('input#chat-new-partner').select2(
                  jQuery.extend(Libertree.UI.selectDefaults,
                                { multiple: false,
                                  width: '100%'
                                })).
                    change(function (event) {
                        fetchConversationWith(event.val, true);
                    });

              syncUIDimensions();
              $('#chat-window').show();
              $('#chat-window .log .messages').scrollTop(999999);
              $('#chat-window .log.active .textarea-chat').focus();
            }
          )
        ;
        Libertree.Chat.rememberDimensions();

        return false;
      } );

      $(document).on('click', '#chat-window .tab', function() {
        var memberId = $(this).data('member-id');
        activateConversation(memberId);
        markConversationSeen(memberId);
      } );

      $(document).on('keydown', '#chat-window .textarea-chat', function(event) {
        if( event.keyCode !== 13 ) {
          return;
        }

        var textarea = $(this),
            memberId = textarea.closest('.log').data('member-id');

        textarea.prop('disabled', true);
        Libertree.UI.TextAreaBackup.disable();

        $.post(
          '/chat/create',
          {
            to_member_id: memberId,
            text: textarea.val()
          },
          function(response) {
            var h = $.parseJSON(response);
            if( h.success ) {
              textarea.val('');
            } else {
              alert('Failed to send chat message.');
            }
            textarea.prop('disabled', false);
          }
        );
      } );

      $(document).on('click', '#chat-window .tab .close', function() {
        var tab = $(this).closest('.tab'),
            memberId = tab.data('member-id'),
            tabToActivate = tab.next();

        if( tabToActivate.length === 0 ) {
          tabToActivate = tab.prev();
        }
        $('#chat-window .tab[data-member-id="'+memberId+'"]').remove();
        $('#chat-window .log[data-member-id="'+memberId+'"]').remove();
        $.get('/chat/closed/'+memberId);
        if( $('#chat-window .tab.active').length === 0 && tabToActivate.length ) {
          activateConversation( tabToActivate.data('member-id') );
        }
        return false;
      } );

      // do this only on wide screens
      if (document.documentElement.clientWidth > Libertree.UI.threshold) {
        $('#chat-window').draggable( {
          handle: '.header',
          stop: function(event, ui) {
            Libertree.Chat.rememberDimensions();
          }
        } );
      }

      $(document).on('click', '#online-contacts .avatar', function() {
        fetchConversationWith( $(this).data('member-id'), true);
        return false;
      } );

      /* ------------------------------------------------------ */

      // do this only on wide screens
      if (document.documentElement.clientWidth > Libertree.UI.threshold) {
        $.cookie('chat-width', $.cookie('chat-width') || 400);
        $.cookie('chat-height', $.cookie('chat-height') || 400);

        $('#chat-window').css( {
          top: $.cookie('chat-top'),
          left: $.cookie('chat-left'),
          width: $.cookie('chat-width'),
          height: $.cookie('chat-height')
        } );
      }

      if( $.cookie('chat-open') === 'true' ) {
        $('#menu-chat').click();
      }

      setInterval( Libertree.Chat.heartbeat, 3 * 60 * 1000 );
    }
  };
}());

$(document).ready( Libertree.Chat.init );
