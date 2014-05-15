/*jshint browser:true, strict:false, curly:false, indent:3*/
(function(){
try {
   var timings = RMARKDOWN_PERFORMANCE_TIMINGS;

   var build_popup = function(popup) {

      // sort the timings in descending order
      timings.sort(function(t1, t2) {
         return t2.elapsed - t1.elapsed;
      });
      
      // emit each to the popup
      for (var i = 0; i < timings.length; i++) {
         var timing = timings[i];
         var perf_timing = document.createElement("div");

         // show the name of the performance marker
         var key_span = document.createElement("span");
         key_span.innerText = timing.marker + ": ";
         perf_timing.appendChild(key_span);

         // show the time elapsesd in ms
         var ms = document.createElement("strong");
         ms.innerText = timing.elapsed + "ms";
         perf_timing.appendChild(ms);

         popup.appendChild(perf_timing);
      }
   };

   var perf_popup = document.createElement("div");
   perf_popup.setAttribute("class", "perf_popup");

   window.addEventListener("keydown", function(evt) {
      var toggle = evt.keyCode == 82 && evt.altKey;
      var dismiss = evt.keyCode == 27;
      if (!toggle && !dismiss)
         return;
      if (perf_popup.parentElement === null && toggle) {
         if (perf_popup.children.length === 0)
            build_popup(perf_popup);
         document.body.appendChild(perf_popup);
      } else {
         document.body.removeChild(perf_popup);
      }
   }, false);
}
catch (e) {
   // ignore failures here; performance timing information is nonessential
   // to page render
}
})();
