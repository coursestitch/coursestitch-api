checkboxChange = function(checkbox, on, off) {
    if (checkbox.checked)
        on();
    else
        off();
};
