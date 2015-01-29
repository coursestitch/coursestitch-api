var formMethods = function(options) {
    options = options || {update: false};

    [].forEach.call(document.querySelectorAll('form'),
    function(form) {
        // Create request
        var request = new XMLHttpRequest();

        // On form submission...
        var onsubmit = form.onsubmit;
        form.onsubmit = function(event) {
            // Stop the form from submitting
            event.preventDefault();
            // Do the onsubmit events
            if (onsubmit)
                onsubmit();
            // Perform request
            request.open(form.attributes.method.value, form.action);
            request.send();
        };

        // When the request is done...
        var loadPage = function() {
            if (options.update) {
                // Update page
                document.body.innerHTML = this.response;

                // Fake history
                window.history.pushState(null, form.attributes.method.value, form.action);
                window.onpopstate = function() {
                    window.location.reload();
                };
            }
        };

        request.addEventListener("load", loadPage, false);
        request.addEventListener("error", loadPage, false);
    });
};

formMethods({update: true});
