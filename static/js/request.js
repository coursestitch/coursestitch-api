request = function(method, uri, data, onload) {
    // Create request
    var request = new XMLHttpRequest();

    // Add callback function for when request is complete
    request.onload = onload;

    // Make request
    request.open(method, uri);

    // Send data
    request.send(data);
};
