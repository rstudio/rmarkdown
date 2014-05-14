/*jshint browser:true, strict:false, curly:false, indent:3*/
(function(){
var timings = RMARKDOWN_PERFORMANCE_TIMINGS;

var build_popup = function(popup) {
   for (var key in timings) {
      var perf_timing = document.createElement("div");
      var key_span = document.createElement("span");
      key_span.innerText = key + ": ";
      perf_timing.appendChild(key_span);
      var ms = document.createElement("strong");
      ms.innerText = timings[key] + "ms";
      perf_timing.appendChild(ms);
      popup.appendChild(perf_timing);
   }
};

var perf_popup = document.createElement("div");
perf_popup.setAttribute("style", 
   "border: 1px solid black; " + 
   "position: fixed;" +
   "top: 20px;" +
   "right: 20px;" + 
   "background-color: #ffffff;" + 
   "padding: 5px; " + 
   "font-size: small;");

window.addEventListener("keydown", function(evt) {
   if (evt.keyCode !== 82 || !evt.altKey) // if not alt+R, ignore
      return;
   if (perf_popup.parentElement === null) {
      if (perf_popup.children.length === 0)
         build_popup(perf_popup);
      document.body.appendChild(perf_popup);
   } else {
      document.body.removeChild(perf_popup);
   }
}, false);

})();
