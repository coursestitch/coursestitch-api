request = function(method, uri, data) {
    // Create request
    var request = new XMLHttpRequest();
    
    // Make request
    request.open(method, uri);

    // Send data
    request.send(data);
};
