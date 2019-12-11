$(function() {
  $("div[class*='section level']").each(function(i, el) {
    var $el = $(el);
    var headers = $el.children().filter(":header");
    for (var h = 0; h < headers.length; h++) {
      var header = headers[h];
      var attrs = header.attributes;
      for (var a = 0; a < attrs.length; a++) {
        var nm = attrs[a].name;
        var val = attrs[a].value;
        nm == "class" ? $el.addClass(val) : $el.attr(nm, val);
      }
    }
  });
});

