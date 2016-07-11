var offset = 15; // left offset of the fixed div (without scrolling)

$(document).scroll(function(e) {
    // b is the fixed div
    $('.well').css({
        'left': offset - $(document).scrollLeft()
    });
});
