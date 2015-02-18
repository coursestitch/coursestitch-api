request = function(method, uri, data, onload, type) {
    // Create request
    var request = new XMLHttpRequest();

    // Add callback function for when request is complete
    request.onload = onload;

    // Make request
    request.open(method, uri);

    // Set request type
    if (type)
        request.setRequestHeader("Accept", type);

    // Send data
    request.send(data);
};
