var formMethods = function(options) {
    options = options || {update: false};

    [].forEach.call(document.querySelectorAll('form'),
    function(form) {
        // Create request
        var request = new XMLHttpRequest();

        // Update the [page with completed request...
        var updatePage = function(options) {
            // Update page
            document.body.innerHTML = this.response;

            // Fake history
            if (options.history)
                window.history.pushState({
                    method: form.attributes.method.value,
                    action: form.action
                }, form.attributes.method.value, form.action);
        };

        // On form submission...
        var onsubmit = form.onsubmit;
        var doRequest = function(event) {
            // Do the onsubmit events
            if (onsubmit)
                onsubmit();
            // Don't do anything for methods that already work
            var method = form.attributes.method.value;
            if (method === "GET" || method == "POST")
                return;
            // Stop the form from submitting
            if (event)
                event.preventDefault();
            // Update the page on request completion 
            if (options.update) {
                var update = function() {
                    // Only replace history if part of onsubmit event
                    if (event)
                        updatePage.call(this, {history: true});
                    // Otherwise just update the page and don't modify the history
                    else
                        updatePage.call(this, {history: false});
                };
                request.addEventListener("load", update, false);
                request.addEventListener("error", update, false);
            }
            // Perform request
            request.open(form.attributes.method.value, form.action);
            request.send(new FormData(form));
        };

        form.onsubmit = doRequest;

        // Deal with fake history
        window.onpopstate = function(event) {
            // If history is a request then replicate the request
            if (event.state && event.state.method)
                doRequest();
            // Otherwise just reload the page
            else
                window.location.reload();
        };
    });
};

formMethods({update: true});
