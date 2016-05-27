

window.initializeProjEmbed = function(filename) {
  $("#proj-download-zip").click(function() {
    var src = window.atob($("#proj-zip").html());
    var blob = new Blob([src], {type: "data:application/zip"});
    saveAs(blob, filename);
  });
};
