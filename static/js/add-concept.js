var addConcept = function(conceptList, conceptId) {
    var url = 'concept/'+conceptId;
    request('GET', url, null, function() {
        var listElement = document.createElement('li');
        listElement.innerHTML = this.responseText;
        conceptList.appendChild(listElement);
    });
};

var createConcept = function(conceptName, topic, callback) {
    request('POST', '/concept?title='+conceptName+'&topic='+topic, null, callback, "application/json");
};

var ENTER_KEY = 13;
conceptInput = function(element) {
    $(element)
        .on('keypress', function(event) {
            var topic = this.dataset.topic;
            if (event.keyCode === ENTER_KEY) {
                createConcept(this.value, topic, function() {
                    var concept = JSON.parse(this.response);
                    addConcept(document.querySelector('#topic-'+topic+'.concept-list'), concept.id);
                });
                this.value = '';
            }
        });
};

$(document).ready(function() {
    conceptInput($('.add-concept'));
});
