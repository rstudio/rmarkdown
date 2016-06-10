

window.initializeSourceEmbed = function(filename) {
  $("#rmd-download-source").click(function() {
    var src = window.atob($("#rmd-source-code").html());

    var uint8 = new Uint8Array(src.length);
    for (var i = 0; i < uint8.length; i++) {
      uint8[i] = src.charCodeAt(i);
    }

    var blob = new Blob([uint8], {type: "text/x-r-markdown"});
    saveAs(blob, filename);
  });
};
