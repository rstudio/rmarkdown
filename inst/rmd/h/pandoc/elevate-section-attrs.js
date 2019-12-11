$(function() {
  $("div.section[class*='level'], section[class*='level']").each(function(i, el) {
    var $section = $(el);
    var headers = $section.children().filter(":header");
    if (headers.length === 0) return;
    var $header = $(headers[0]);
    var attrs = headers[0].attributes;
    for (var a = 0; a < attrs.length; a++) {
      var nm = attrs[a].name;
      var val = attrs[a].value;
      if (nm == "class") {
        $section.addClass(val);
        $header.removeClass(val);
        continue;
      }
      $section.attr(nm, val);
      $header.attr(nm, null);
    }
  });
});

