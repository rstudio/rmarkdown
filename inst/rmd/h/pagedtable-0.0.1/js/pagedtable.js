var PagedTable;
(function (PagedTable) {
  var pageSize = 10;

  var headersFromJson = function(json) {
    if (json === null || json.length === 0)
      return [];

    var columns = Object.keys(json[0]);
    if (columns.length > 10) {
      columns = columns.slice(1, 10);
      columns[10] = "...";
    }

    return columns;
  };

  var renderHeader = function(pagedTable) {
    var data = getDataFromPagedTable(pagedTable);

    var thead = pagedTable.querySelectorAll("thead")[0];
    thead.innerHTML = "";

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

  var renderBody = function(pagedTable) {
    var pageNumber = getPageNumber(pagedTable);
    var data = getDataFromPagedTable(pagedTable);

    var tbody = pagedTable.querySelectorAll("tbody")[0];
    tbody.innerHTML = "";

    var headerNames = headersFromJson(data);

    var pageData = data.slice(pageNumber * pageSize, (pageNumber + 1) * pageSize);

    pageData.forEach(function(dataRow, idxRow) {
      var htmlRow = document.createElement("tr");
      htmlRow.setAttribute("class", (idxRow % 2 !==0) ? "even" : "odd");

      headerNames.forEach(function(cellName) {
        var dataCell = cellName === "..." ? "" : dataRow[cellName];
        var htmlCell = document.createElement("td");
        htmlCell.appendChild(document.createTextNode(dataCell));
        htmlRow.appendChild(htmlCell);
      });

      tbody.appendChild(htmlRow);
    });

    return tbody;
  };

  var renderFooter = function(pagedTable) {
    var pageNumber = getPageNumber(pagedTable);
    var data = getDataFromPagedTable(pagedTable);

    var footer = pagedTable.querySelectorAll("div.pagedtable-page-footer")[0];
    footer.innerHTML = "";

    var previous = document.createElement("a");
    previous.appendChild(document.createTextNode("previous"));
    previous.onclick = function() {
      increasePageNumber(pagedTable, -1);
      renderBody(pagedTable);
      renderFooter(pagedTable);
    };
    footer.appendChild(previous);

    var next = document.createElement("a");
    next.appendChild(document.createTextNode("next"));
    next.onclick = function() {
      increasePageNumber(pagedTable, 1);
      renderBody(pagedTable);
      renderFooter(pagedTable);
    };
    footer.appendChild(next);

    previous.setAttribute("class", pageNumber <= 0 ? "prev disabled" : "prev enabled");
    next.setAttribute("class", (pageNumber + 1) * pageSize >= data.length - 1 ? "next disabled" : "next enabled");
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

  var getPageNumber = function(pagedTable) {
    return parseInt(pagedTable.getAttribute("pagedtable-page"));
  };

  var increasePageNumber = function(pagedTable, increase) {
    var newPageNumber = getPageNumber(pagedTable) + increase;
    var data = getDataFromPagedTable(pagedTable);

    if (newPageNumber < 0) newPageNumber = 0;
    if (newPageNumber * pageSize >= data.length) newPageNumber = newPageNumber - increase;

    pagedTable.setAttribute("pagedtable-page", newPageNumber);
  };

  var renderOne = function(pagedTable) {
    var tableDiv = document.createElement("div");
    pagedTable.appendChild(tableDiv);
    tableDiv.setAttribute("class", "pagedtable");

    var table = document.createElement("table");
    table.setAttribute("class", "table table-condensed");
    tableDiv.appendChild(table);

    table.appendChild(document.createElement("thead"));
    table.appendChild(document.createElement("tbody"));

    var footerDiv = document.createElement("div");
    footerDiv.setAttribute("class", "pagedtable-page-footer");
    tableDiv.appendChild(footerDiv);

    renderHeader(pagedTable);
    renderBody(pagedTable);
    renderFooter(pagedTable);
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
