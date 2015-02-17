var addTopic = function(topicList, topicId) {
    var url = 'topic/'+topicId;
    request('GET', url, null, function() {
        var listElement = document.createElement('li');
        listElement.innerHTML = this.responseText;
        topicList.appendChild(listElement);
    });
};

$(document).ready(function() {
    $('.add-topic').on('typeahead:selected', function(event, data) {
        addTopic(document.querySelector('.topic-list'), data['id'])
    });
});
