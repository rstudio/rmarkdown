var PagedTable;
(function (PagedTable) {
  var pageSize = 10;

  var headersFromJson = function(json) {
    if (json === null || json.length === 0)
      return [];

    return Object.keys(json[0]);
  };

  var renderHeader = function(data) {
    var thead = document.createElement("thead");

    var header = document.createElement("tr");
    thead.appendChild(header);

    var headerNames = headersFromJson(data);
    headerNames.forEach(function(headerName) {
      var column = document.createElement("th");
      column.setAttribute("style", "text-align: right");

      column.appendChild(document.createTextNode(headerName));
      header.appendChild(column);
    });

    return thead;
  };

  var renderBody = function(data, pageNumber) {
    var tbody = document.createElement("tbody");
    var headerNames = headersFromJson(data);

    var pageData = data.slice(pageNumber * pageSize, (pageNumber + 1) * pageSize);

    pageData.forEach(function(dataRow, idxRow) {
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

    return tbody;
  };

  var getDataFromPagedTable = function(pagedTableId) {
    var pagedTable = document.getElementById(pagedTableId);

    var sourceElems = [].slice.call(pagedTable.children).filter(function(e) {
      return e.hasAttribute("data-pagedtable-source");
    });

    if (sourceElems === null || sourceElems.length !== 1) {
      throw("A single data-pagedtable-source was not found");
    }

    return JSON.parse(sourceElems[0].innerHTML);
  };

  var renderOne = function(pagedTableId, pageNumber) {
    var pagedTable = document.getElementById(pagedTableId);
    var data = getDataFromPagedTable(pagedTableId);

    var tableDiv = document.createElement("div");
    tableDiv.setAttribute("class", "pagedtable");

    var table = document.createElement("table");
    table.setAttribute("class", "table table-condensed");
    tableDiv.appendChild(table);

    var thead = renderHeader(data);
    table.appendChild(thead);

    var tbody = renderBody(data, pageNumber);
    table.appendChild(tbody);

    pagedTable.appendChild(tableDiv);
  };

  PagedTable.renderAll = function() {
    var pagedTables = document.querySelectorAll('[data-pagedtable]');
    pagedTables.forEach(function(pagedTable, idx) {
      pagedTable.id = pagedTable.id === "" ? "pagedtable-" + idx : pagedTable.id;
      renderOne(pagedTable.id, 0);
    });
  };

  PagedTable.setPageSize = function(newPageSize) {
    pageSize = newPageSize;
  };

  return PagedTable;
})(PagedTable || (PagedTable = {}));

window.onload = function() {
  PagedTable.renderAll();
};
