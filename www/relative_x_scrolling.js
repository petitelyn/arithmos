var offset = 15; // left offset of the fixed div (without scrolling)

$(document).scroll(function(e) {
    // b is the fixed div
    $('.well').css({
        'left': offset - $(document).scrollLeft()
    });
});

var leftOffset = parseInt($("#restart").css('left')); //Grab the left position left first
$(window).scroll(function(){
    $('#restart').css({
        'left': $(this).scrollLeft() + leftOffset //Use it later
    });
});