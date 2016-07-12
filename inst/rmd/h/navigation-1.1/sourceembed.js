//fix zip download bug, https://github.com/eligrey/FileSaver.js/issues/156
function str2bytes (str) {
    var bytes = new Uint8Array(str.length);
    for (var i=0; i<str.length; i++) {
        bytes[i] = str.charCodeAt(i);
    }
    return bytes;
}

window.initializeSourceEmbed = function(filename, type) {
  $(".rmd-download-source").click(function() {
    var src = window.atob($("#rmd-source-code").html());
    var blob = new Blob([str2bytes(src)], {type: type});
    saveAs(blob, filename);
  });
};
