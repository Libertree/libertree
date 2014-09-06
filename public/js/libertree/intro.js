/*jslint white: true, indent: 2, todo: true */
/*global $, Libertree, console */

Libertree.Intro = (function () {
  "use strict";

  var currentStep = function(that) {
      return $(that).closest('.tutorial-step');
    },

    nextStep = function(step) {
      var errorContainer = $(step).find('.error'),
          next_id = "#step-" + $(step).find('.button.next').data('next');

      // clear errors
      if (errorContainer) {
        errorContainer.html("");
      }

      // show next step
      step.hide();
      $(next_id).show();
      window.location = next_id;
    },

    restoreStep = function(step) {
      Libertree.UI.removeSpinner(step);
      $(step).find('.button').show();
    },

    forward = function(step, element) {
      if ($(element).data('next') === undefined) {
        window.location = $(element).prop('href');
      } else {
        nextStep(step);
      }
    },

    // observe a function's return value
    evaluateResponse = function(result, step, that) {
      var message = $(step).data('success'),
          errorContainer = $(step).find('.errors');

      // interpret result as object
      if (typeof result === 'object' && result.responseText) {
        result = JSON.parse(result.responseText);
      }

      // success?
      if(result.status === 'success') {
        // display success message in next step if defined
        if (message !== undefined && message !== "") {
          $(step).next().find('h1').after("<p class='message'>"+message+"</p>");
        }
        forward(step, that);
      } else if (result.status === 'skip') {
        forward(step, that);
      } else {
        // display errors
        if (errorContainer) {
          errorContainer.html(result.msg);
        } else {
          console.log(result.msg);
        }
      }
      restoreStep(step);
    },

    /* button click handler functions */
    // go to next step without executing functions
    skipHandler = function(event) {
      event.preventDefault();
      nextStep( currentStep(this) );
    },

    // unhide the previous and hide the current step
    prevHandler = function() {
      var prev_id = "#step-" + $(this).data('prev');
      currentStep(this).hide();
      $(prev_id).show();
    },

    // Execute a function (if provided).
    // Then, unhide the next and hide the current step.
    nextHandler = function(event) {
      event.preventDefault();

      var step = currentStep(this),
          result,
          that = this;

      // execute function if provided and valid
      if (step.data('func') && Libertree.Intro[step.data('func')] !== undefined) {
        Libertree.UI.addSpinner(this, 'after');
        $(step).find('.button').hide();

        // execute specified function
        result = Libertree.Intro[step.data('func')](this);

        // If the return value is evaluated asynchronously, wait for it
        // TODO: I don't like this. Pass the function to a handler that does all of this instead?
        if (typeof result.promise === "function") {
          result.promise().done(
            function() {
              evaluateResponse(result, step, that);
            });
        } else {
          evaluateResponse(result, step, that);
        }
      } else {
        // end tutorial or move on to next step
        forward(step, that);
      }
    };

  return {
    currentStep: currentStep,

    /* step 2 --------------------------------------------------*/
    createRiver: function(that) {
      // get river query from input field
      var query = $(that.id+' #first-river-query').val();

      // don't create river for empty query
      if (query === "") {
        return {'status': 'skip'};
      }

      return $.post(
        '/rivers/_create_tutorial_river',
        { query: query }
      );
    },

    /* step 3 --------------------------------------------------*/
    addRiversFromList: function(that) {
      var step = Libertree.Intro.currentStep(that),
        objectify = function(i,e) {
          if ($(e).prop('checked')) {
            return {
              'query': $(e).data('query'),
              'label': $(e).data('label')
            };
          }
        },
        rivers = $(step).find('input').map(objectify).get();

      // don't make a request if nothing is selected
      if (rivers.length === 0) {
        return {'status':'success'};
      }

      return $.post(
        '/rivers/_create_default_rivers',
        { rivers: rivers }
      );
    },

    /* step 4 --------------------------------------------------*/
    createContactList: function(that) {
      var step = Libertree.Intro.currentStep(that),
          members = $(that.id+' #contact-list-members').val();

      // don't create contact list if no members specified
      if( members === null ) {
        return {'status': 'skip'};
      }

      return $.post(
        '/contact-lists/create.json',
        {
          name: $(step).data('name'),
          members: members,
          intro: true
        }
      );
    },

    init: function() {
      // unhide the step that is indicated in the URL, or unhide the first step
      // TODO: show and hide steps when the hash in the location changes

      var step = window.location.hash;
      if (step) {
        $(step).show();
      } else {
        $('.tutorial-step').first().show();
      }

      // bootstrap popovers for additional information
      $("a[rel=popover]")
        .popover()
        .click(function() {
          return false;
        });
      $(document).click( function() {
        // hide all popovers
        $("a[rel=popover]").popover('hide');
      });

      // enable fancy contact list member selector
      $('input#contact-list-members').select2(Libertree.UI.selectDefaults);

      $(document).on('click', '.tutorial-step .button.prev', prevHandler);
      $(document).on('click', '.tutorial-step .button.skip', skipHandler);
      $(document).on('click', '.tutorial-step .button.next', nextHandler);
    }
  };
}());

$(document).ready( Libertree.Intro.init );
