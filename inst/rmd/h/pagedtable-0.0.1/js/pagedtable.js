function headersFromJson(json) {
  if (json === null || json.length === 0)
    return [];

  return Object.keys(json[0]);
}

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
      var sourceData = JSON.parse(sourceElems[0].innerHTML);

      var table = document.createElement("table");
      table.setAttribute("class", "pagedtable")

      var header = document.createElement("tr");
      table.appendChild(header);

      var headerNames = headersFromJson(sourceData);
      headerNames.map(function(headerName) {
        var column = document.createElement("th");
        column.appendChild(document.createTextNode(headerName));
        header.appendChild(column);
      })

      pagedTable.appendChild(table);
    }
  })
}


window.onload = function() {
  renderPagedTables();
};
