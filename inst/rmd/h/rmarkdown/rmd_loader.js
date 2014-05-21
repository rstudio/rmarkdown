/*jshint browser:true, strict:false, curly:false, indent:3*/
(function(){
try {
window.setTimeout(function() {
   // if the document has loaded in an error state after the loader 
   // appears, hide the loader gracefully
   window.setInterval(function() {
      if ($("#__reactivedoc__").hasClass("shiny-output-error")) 
         $("#rmd_loader").fadeOut(400);
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
