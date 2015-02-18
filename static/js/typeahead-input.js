var typeahead = function (element) {
    element = $(element);
    var url = element.data('url') || '';
    var key = element.data('key') || 'title';

    // Define the contents of the typahead
    var contents = new Bloodhound({
        datumTokenizer: Bloodhound.tokenizers.obj.whitespace(key),
        queryTokenizer: Bloodhound.tokenizers.whitespace,
        limit: 10,
        prefetch: {
            url: url,
        },
    });
    
    // Fetch the contents
    contents.initialize();
     
    // Create the typeahead element
    element.typeahead(null, {
        displayKey: key,
        source: contents.ttAdapter(),
    });
};

$(document).ready(function() {
    $('.typeahead').each(function(i, element) {
        typeahead(element);
    });
});
