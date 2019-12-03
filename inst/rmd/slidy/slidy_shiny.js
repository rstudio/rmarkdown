(function() {
  if (!window.w3c_slidy) return;
  if (!window.Shiny) return;
  if (!window.$) return;
  // whenever a slide changes, tell shiny to recalculate what is displayed
  window.w3c_slidy.add_observer(function () {
    $(document.body).children().first().trigger("shown");
  });
})()
