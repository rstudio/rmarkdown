function renderPagedTables() {
  var pagedTables = document.querySelectorAll('[data-pagedtable]');
  pagedTables.forEach(function(pagedTable) {
    var sourceName = pagedTable.getAttribute("data-pagedtable");
    var sourceElems = [].slice.call(pagedTable.children).filter(function(e) {
      return e.hasAttribute("data-pagedtable-source")
    });
    if (sourceElems === null || sourceElems.length !== 1) {
      pagedTable.innerHTML = "Error: A single data-pagedtable-source was not found";
    }
    else {
      var sourceElem = sourceElems[0];

      pagedTable.innerHTML = sourceElem.innerHTML
    }
  })
}


window.onload = function() {
  renderPagedTables();
};
