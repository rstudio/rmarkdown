$(function() {
  $("div[class*='section level']").each(function(i, el) {
    var $el = $(el);
    var headers = $el.children().filter(":header");
    for (var h = 0; h < headers.length; h++) {
      var header = headers[h];
      var attrs = header.attributes;
      for (var a = 0; a < attrs.length; a++) {
        $el.attr(attrs[a].name, attrs[a].value);
      }
    }
  });
});

