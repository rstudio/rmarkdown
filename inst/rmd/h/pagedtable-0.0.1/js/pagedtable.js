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

  var getDataFromPagedTable = function(pagedTable) {
    var sourceElems = [].slice.call(pagedTable.children).filter(function(e) {
      return e.hasAttribute("data-pagedtable-source");
    });

    if (sourceElems === null || sourceElems.length !== 1) {
      throw("A single data-pagedtable-source was not found");
    }

    return JSON.parse(sourceElems[0].innerHTML);
  };

  var getTableDiv = function(pagedTable) {
    var selection = pagedTable.querySelectorAll("div.pagedtable");

    var tableDiv = null;
    if (selection.length === 0) {
      tableDiv = document.createElement("div");
      pagedTable.appendChild(tableDiv);
      tableDiv.setAttribute("class", "pagedtable");
    }
    else {
      tableDiv = selection[0];
    }

    return tableDiv;
  };

  var getPageNumber = function(pagedTable) {
    return parseInt(pagedTable.getAttribute("pagedtable-page"));
  };

  var increasePageNumber = function(pagedTable, increase) {
    var newPageNumber = getPageNumber(pagedTable) + increase;
    pagedTable.setAttribute("pagedtable-page", newPageNumber);
  };

  var renderOne = function(pagedTable) {
    var pageNumber = getPageNumber(pagedTable);

    var data = getDataFromPagedTable(pagedTable);

    var tableDiv = getTableDiv(pagedTable);
    tableDiv.innerHTML = "";

    var table = document.createElement("table");
    table.setAttribute("class", "table table-condensed");
    tableDiv.appendChild(table);

    table.appendChild(renderHeader(data));
    table.appendChild(renderBody(data, pageNumber));
    table.appendChild(renderFooter(pagedTable, pageNumber));
  };

  var renderFooter = function(pagedTable) {
    var footer = document.createElement("tfoot");

    var row = document.createElement("tr");
    footer.appendChild(row);

    var cell = document.createElement("td");
    cell.setAttribute("colspan", "100%");
    row.appendChild(cell);

    var previous = document.createElement("a");
    previous.appendChild(document.createTextNode("previous"));
    previous.onclick = function() {
      increasePageNumber(pagedTable, -1);
      renderOne(pagedTable);
    };
    cell.appendChild(previous);

    var next = document.createElement("a");
    next.appendChild(document.createTextNode("next"));
    next.onclick = function() {
      increasePageNumber(pagedTable, 1);
      renderOne(pagedTable);
    };
    cell.appendChild(next);

    return footer;
  };

  PagedTable.renderAll = function() {
    var pagedTables = document.querySelectorAll('[data-pagedtable]');
    pagedTables.forEach(function(pagedTable, idx) {
      pagedTable.setAttribute("pagedtable-page", 0);
      renderOne(pagedTable);
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
