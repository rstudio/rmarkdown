$(function() {
  $("div[class*='section level']").each(function(i, el) {
    var $el = $(el);
    var headers = $el.children().filter(":header");
    if (headers.length === 0) return;
    // TODO: do we need to do this for more attributes (not just class)?
    // Also, should we be removing the attributes on the headers?
    $el.addClass(headers.attr("class"));
  });
});

