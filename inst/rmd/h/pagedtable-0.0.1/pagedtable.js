function renderPagedTables() {
  var pagedTables = document.querySelectorAll('[data-pagedtable]');
  pagedTables.forEach(function(pagedTable) {
    var sourceName = pagedTable.getAttribute("data-pagedtable");
    var sourceElem = document.getElementById(sourceName);
    if (sourceElem === null) {
      pagedTable.innerHTML = "Error: HTML element with id '" + sourceName + "' was not found";
    }
    else {
      pagedTable.innerHTML = sourceElem.innerHTML
    }
  })
}


window.onload = function() {
  renderPagedTables();
};
