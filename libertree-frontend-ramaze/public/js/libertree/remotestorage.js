Libertree.RemoteStorage = (function () {
    var path = "/public/libertree/";

    /* This is a hack.  The remote storage provider sends the
       access token in the location hash, so we cannot simply
       take it from the HTTP request.  This function forwards the
       access token as a GET request parameter. */
    $(document).ready(function () {
        var match = window.location.hash.match(/access_token=(.*)/);
        if (match) {
            window.location.replace('?access_token='+match[1]);
        }
    });

    function upload (form, cb) {
        var reader = new FileReader();
        var file = form.find('#fileToUpload').get(0).files[0];
        var shipOff = function(event) {
            remoteStorage.wireClient.setStorageInfo({
                type: 'remotestorage-00',
                href: form.data('remote-url')
            });
            remoteStorage.wireClient.setBearerToken(form.data('access-token'));
            // TODO: try to make the file name unique
            remoteStorage.wireClient.set(
                path + file.name,
                event.target.result,
                file.type,
                null // vestigial callback in remoteStorage.js
            ).then(
                // success handler
                function () {
                    var msgbox = form.find('.message');
                    msgbox.text( msgbox.data('msg-success') );
                    msgbox.removeClass('alert-error');
                    msgbox.addClass('alert-info');
                    msgbox.show();

                    remoteStorage.wireClient.get(path).
                        then(function (response) {
                            // TODO: replace file list
                            console.log(response);
                        });
                },
                // error handler
                function (error) {
                    var msgbox = form.find('.message');
                    msgbox.text( msgbox.data('msg-error') + error );
                    msgbox.removeClass('alert-info');
                    msgbox.addClass('alert-error');
                    msgbox.show();
                }
            ).then(cb);
        };
        reader.readAsArrayBuffer(file);
        reader.onload = shipOff;
    }

    $(document).on('click', '#upload input[type="submit"]', function(event) {
        var submitButton = $(this),
            form = $(submitButton).closest('form'),
            resetForm = function () {
                submitButton.prop('disabled', false);
                Libertree.UI.removeSpinner(form);
                form.get(0).reset();
            };

        // check whether the File API is supported
        var supported = form.find('#fileToUpload').get(0).files !== undefined;

        if (supported) {
            event.preventDefault();
            submitButton.prop('disabled', true);
            Libertree.UI.addSpinner(form, 'append', 16);
            upload(form, resetForm);
        }
    });
}());
