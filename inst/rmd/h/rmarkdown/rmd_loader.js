/*jshint browser:true, strict:false, curly:false, indent:3*/
(function(){
try {
window.setTimeout(function() {
   // if the document has loaded in an error state after the loader 
   // appears, hide the loader gracefully
   var intervalId = window.setInterval(function() {
      if ($("#__reactivedoc__").hasClass("shiny-output-error")) 
         $("#rmd_loader").fadeOut(400);
      // once loading is finished (which Shiny indicates by hiding the 
      // rmd_loader), initialize slidy if it exists, and stop the timer
      if (!$("#rmd_loader").is(":visible")) {
         // initialize slidy after load completes
         if (typeof window.w3c_slidy != "undefined") {
            w3c_slidy.mouse_click_enabled = false; // interferes with Shiny input
            w3c_slidy.init();
         }
         window.clearInterval(intervalId);
      }
   }, 500);
   $("#rmd_loader").fadeIn(400);
},
// fade the loader into view after 200 ms
200);
}
catch (e) {
   // ignore failures here; the loading status is nonessential 
}
})();
