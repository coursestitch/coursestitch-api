var addTopic = function(topicList, topicId) {
    var url = 'topic/'+topicId;
    request('GET', url, null, function() {
        var listElement = document.createElement('li');
        listElement.innerHTML = this.responseText;
        topicList.appendChild(listElement);
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
        })
        .on('keypress', function(event, data) {
            if (event.keyCode === ENTER_KEY)
                createTopic(this.value, function() {
                    var topic = JSON.parse(this.response);
                    addTopic(document.querySelector('.topic-list'), topic.id)
                });
        });
});
