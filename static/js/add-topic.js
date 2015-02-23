var addTopic = function(topicList, topicId) {
    var url = 'topic/'+topicId;
    request('GET', url, null, function() {
        var listElement = document.createElement('li');
        listElement.innerHTML = this.responseText;
        topicList.appendChild(listElement);
        // Make concept input functional
        var addConcept = $(listElement).children('.add-concept');
        conceptInput(addConcept);
    });
};

var createTopic = function(topicName, callback) {
    request('POST', '/topic?title='+topicName+'&summary', null, callback, "application/json");
};

var ENTER_KEY = 13;
$(document).ready(function() {
    $('.add-topic')
        .on('typeahead:selected', function(event, data) {
            addTopic(document.querySelector('.topic-list'), data['id'])
            this.value = '';
        })
        .on('typeahead:autocompleted', function(event, data) {
            $(this).data('id', data['id']);
        })
        .on('keypress', function(event, data) {
            if (event.keyCode === ENTER_KEY) {
                var id = $(this).data('id');
                if (id) {
                    addTopic(document.querySelector('.topic-list'), id);
                    this.value = '';
                    $(this).typeahead('close');
                } else {
                    createTopic(this.value, function() {
                        var topic = JSON.parse(this.response);
                        addTopic(document.querySelector('.topic-list'), topic.id)
                    });
                    this.value = '';
                }
            } else {
                $(this).data('id', undefined);
            }
        });
});
