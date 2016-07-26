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

      var tableDiv = document.createElement("div");
      tableDiv.setAttribute("class", "pagedtable")

      var table = document.createElement("table");
      table.setAttribute("class", "table table-condensed")
      tableDiv.appendChild(table);

      var thead = document.createElement("thead");
      table.appendChild(thead);

      var header = document.createElement("tr");
      thead.appendChild(header);

      var headerNames = headersFromJson(sourceData);
      headerNames.forEach(function(headerName) {
        var column = document.createElement("th");
        column.setAttribute("style", "text-align: right")

        column.appendChild(document.createTextNode(headerName));
        header.appendChild(column);
      })

      var tbody = document.createElement("tbody");
      table.appendChild(tbody);

      sourceData.forEach(function(dataRow, idxRow) {
        var htmlRow = document.createElement("tr");
        htmlRow.setAttribute("class", (idxRow % 2 !==0) ? "even" : "odd");

        headerNames.forEach(function(cellName) {
          var dataCell = dataRow[cellName];
          var htmlCell = document.createElement("td");
          htmlCell.appendChild(document.createTextNode(dataCell));
          htmlRow.appendChild(htmlCell);
        });

        tbody.appendChild(htmlRow);
      });

      pagedTable.appendChild(tableDiv);
    }
  })
}


window.onload = function() {
  renderPagedTables();
};
