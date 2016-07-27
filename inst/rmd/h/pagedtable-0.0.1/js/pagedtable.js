var PagedTable = function (pagedTable) {
  var pageSize = 10;
  var pageNumber = 0;
  var pageCount;
  var data;

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

  var renderHeader = function() {
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

  var renderBody = function() {
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

  var getVisiblePageRange = function() {
    var start = pageNumber - 4;
    var end = pageNumber + 5;

    if (start < 0) {
      var diffToStart = 0 - start;
      start += diffToStart;
      end += diffToStart;
    }

    if (end > pageCount) {
      var diffToEnd = end - pageCount;
      start -= diffToEnd;
      end -= diffToEnd;
    }

    start = start < 0 ? 0 : start;
    end = end >= pageCount ? pageCount : end;

    return {
      start: start,
      end: end
    };
  };

  var renderFooter = function() {
    var footer = pagedTable.querySelectorAll("div.pagedtable-footer")[0];
    footer.innerHTML = "";

    var next = document.createElement("a");
    next.appendChild(document.createTextNode("next"));
    next.onclick = function() {
      setPageNumber(pageNumber + 1);
      renderBody(pagedTable);
      renderFooter(pagedTable);
    };
    footer.appendChild(next);

    var pageNumbers = document.createElement("div");
    pageNumbers.setAttribute("class", "pagedtable-indexes");

    var pageRange = getVisiblePageRange();
    for (var idxPage = pageRange.start; idxPage < pageRange.end; idxPage++) {
      var pageLink = document.createElement("a");
      pageLinkClass = idxPage === pageNumber ? "pagedtable-index pagedtable-index-current" : "pagedtable-index";
      pageLink.setAttribute("class", pageLinkClass);
      pageLink.setAttribute("data-page-index", idxPage);
      pageLink.onclick = function() {
        setPageNumber(parseInt(this.getAttribute("data-page-index")));
        renderBody(pagedTable);
        renderFooter(pagedTable);
      };

      pageLink.appendChild(document.createTextNode(idxPage + 1));
      pageNumbers.appendChild(pageLink);
    }
    footer.appendChild(pageNumbers);

    var previous = document.createElement("a");
    previous.appendChild(document.createTextNode("previous"));
    previous.onclick = function() {
      setPageNumber(pageNumber - 1);
      renderBody(pagedTable);
      renderFooter(pagedTable);
    };
    footer.appendChild(previous);

    var enabledClass = "pagedtable-index-nav";
    var disabledClass = "pagedtable-index-nav pagedtable-index-nav-disabled";
    previous.setAttribute("class", pageNumber <= 0 ? disabledClass : enabledClass);
    next.setAttribute("class", (pageNumber + 1) * pageSize >= data.length - 1 ? disabledClass : enabledClass);
  };

  var getDataFromPagedTable = function() {
    var sourceElems = [].slice.call(pagedTable.children).filter(function(e) {
      return e.hasAttribute("data-pagedtable-source");
    });

    if (sourceElems === null || sourceElems.length !== 1) {
      throw("A single data-pagedtable-source was not found");
    }

    return JSON.parse(sourceElems[0].innerHTML);
  };

  var setPageNumber = function(newPageNumber) {
    if (newPageNumber < 0) return;
    if (newPageNumber * pageSize >= data.length) return;

    pageNumber = newPageNumber;
  };

  var getPageCount = function() {
    return Math.ceil(data.length / pageSize);
  };

  this.render = function() {
    var tableDiv = document.createElement("div");
    pagedTable.appendChild(tableDiv);
    tableDiv.setAttribute("class", "pagedtable");

    var table = document.createElement("table");
    table.setAttribute("class", "table table-condensed");
    tableDiv.appendChild(table);

    table.appendChild(document.createElement("thead"));
    table.appendChild(document.createElement("tbody"));

    var footerDiv = document.createElement("div");
    footerDiv.setAttribute("class", "pagedtable-footer");
    tableDiv.appendChild(footerDiv);

    renderHeader(pagedTable);
    renderBody(pagedTable);
    renderFooter(pagedTable);
  };

  var init = function() {
    data = getDataFromPagedTable(pagedTable);
    pageCount = getPageCount();
  };

  init();
};

var PagedTableDoc;
(function (PagedTableDoc) {
  PagedTableDoc.renderAll = function() {
    var pagedTables = document.querySelectorAll('[data-pagedtable]');
    pagedTables.forEach(function(pagedTable, idx) {
      pagedTable.setAttribute("pagedtable-page", 0);

      var pagedTableInstance = new PagedTable(pagedTable);
      pagedTableInstance.render();
    });
  };

  return PagedTableDoc;
})(PagedTableDoc || (PagedTableDoc = {}));

window.onload = function() {
  PagedTableDoc.renderAll();
};
