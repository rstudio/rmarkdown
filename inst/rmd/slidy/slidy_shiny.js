(function() {
  if (!window.w3c_slidy) return;
  if (!window.Shiny) return;
  if (!window.$) return;
  // whenever a slide changes, tell shiny to recalculate what is displayed
  window.w3c_slidy.add_observer(function (slide_num) {
    // slide_num starts at position 1
    $(w3c_slidy.slides[slide_num - 1]).trigger("shown");
  });
})()
