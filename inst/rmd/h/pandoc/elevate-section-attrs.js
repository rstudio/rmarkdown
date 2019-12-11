$(function() {
  $("div.section[class*='level'], section[class*='level']").each(function(i, el) {
    var $section = $(el);
    var $header = $section.children().filter(":header").first();
    if ($header.length === 0) return;
    var attrs = $header[0].attributes;
    for (var a = 0; a < attrs.length; a++) {
      var nm = attrs[a].name;
      var val = attrs[a].value;
      if (nm === "class") {
        $section.addClass(val);
        $header.removeClass(val);
        continue;
      }
      $section.attr(nm, val);
      $header.attr(nm, null);
    }
  });
});

