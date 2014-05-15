/*jshint browser:true, strict:false, curly:false, indent:3*/
(function(){
try {
   var timings = RMARKDOWN_PERFORMANCE_TIMINGS;

   var build_popup = function(popup) {
      // sort the timings in descending order
      timings.sort(function(t1, t2) {
         return t2.elapsed - t1.elapsed;
      });
      
      var longest = timings[0].elapsed;

      // emit each to the popup
      for (var i = 0; i < timings.length; i++) {
         var timing = timings[i];
         var perf_timing = document.createElement("div");

         // show the name of the performance marker
         var key_span = document.createElement("span");
         key_span.innerText = timing.marker + ": ";
         perf_timing.appendChild(key_span);

         // show the time elapsed in ms
         var ms = document.createElement("strong");
         ms.innerText = timing.elapsed + "ms";
         perf_timing.appendChild(ms);

         popup.appendChild(perf_timing);

         // show a bar 
         var databar = document.createElement("div");
         databar.setAttribute("class", "perf_databar");
         databar.style.width = 
            Math.round((timing.elapsed / longest) * 100) + "%";
         popup.appendChild(databar);
      }
   };

   // create the popup element (unattached unless shown)
   var perf_popup = document.createElement("div");
   perf_popup.setAttribute("class", "perf_popup");

   // toggle the popup with Alt+R; if up, Esc dismisses
   window.addEventListener("keydown", function(evt) {
      var toggle = evt.keyCode == 82 && evt.altKey; // Alt+R
      var dismiss = evt.keyCode == 27; // Esc
      if (!toggle && !dismiss)
         return;
      if (perf_popup.parentElement === null && toggle) {
         // rebuild the popup if it's empty
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
