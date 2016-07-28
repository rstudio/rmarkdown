var PagedTable = function (pagedTable) {
  var pageSize = 10;
  var pageNumber = 0;
  var pageCount;
  var data;
  var types;

  var columnsFromJson = function(json) {
    if (json === null || json.length === 0)
      return [];

    var columns = Object.keys(json[0]);
    if (columns.length > 10) {
      columns = columns.slice(1, 10);
      columns[10] = "...";
    }

    var typedColumns = columns.map(function(columnName, columnIdx) {
      return {
        name: columnName,
        type: types[columnIdx]
      }
    });

    return typedColumns;
  };

  var renderHeader = function() {
    var thead = pagedTable.querySelectorAll("thead")[0];
    thead.innerHTML = "";

    var header = document.createElement("tr");
    thead.appendChild(header);

    var columnsData = columnsFromJson(data);
    columnsData.forEach(function(columnData) {
      var column = document.createElement("th");
      column.setAttribute("style", "text-align: right");

      column.appendChild(document.createTextNode(columnData.name));
      header.appendChild(column);
    });

    return thead;
  };

  var renderBody = function() {
    var tbody = pagedTable.querySelectorAll("tbody")[0];
    tbody.innerHTML = "";

    var columnsData = columnsFromJson(data);

    var pageData = data.slice(pageNumber * pageSize, (pageNumber + 1) * pageSize);

    pageData.forEach(function(dataRow, idxRow) {
      var htmlRow = document.createElement("tr");
      htmlRow.setAttribute("class", (idxRow % 2 !==0) ? "even" : "odd");

      columnsData.forEach(function(columnData) {
        var cellName = columnData.name;
        var dataCell = cellName === "..." ? "" : dataRow[cellName];
        var htmlCell = document.createElement("td");
        htmlCell.appendChild(document.createTextNode(dataCell));
        htmlCell.setAttribute("class", "pagedtable-type-" + columnData.type);
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

  var getLabelInfo = function() {
    var pageStart = pageNumber * pageSize + 1;
    var pageEnd = Math.min((pageNumber + 1) * pageSize, data.length);
    var totalRecods = data.length;

    var infoText = pageStart + "-" + pageEnd + " of " + totalRecods;
    if (totalRecods < pageSize) {
      infoText = totalRecods + " Records";
    }

    return infoText;
  };

  var renderFooter = function() {
    var footer = pagedTable.querySelectorAll("div.pagedtable-footer")[0];
    footer.innerHTML = "";

    var next = document.createElement("a");
    next.appendChild(document.createTextNode("Next"));
    next.onclick = function() {
      setPageNumber(pageNumber + 1);
      renderBody(pagedTable);
      renderFooter(pagedTable);
    };
    if (data.length > pageSize) footer.appendChild(next);

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
    previous.appendChild(document.createTextNode("Previous"));
    previous.onclick = function() {
      setPageNumber(pageNumber - 1);
      renderBody(pagedTable);
      renderFooter(pagedTable);
    };
    if (data.length > pageSize) footer.appendChild(previous);

    var infoLabel = document.createElement("div");
    infoLabel.setAttribute("class", "pagedtable-info");
    infoLabel.appendChild(document.createTextNode(getLabelInfo()));
    footer.appendChild(infoLabel);

    var enabledClass = "pagedtable-index-nav";
    var disabledClass = "pagedtable-index-nav pagedtable-index-nav-disabled";
    previous.setAttribute("class", pageNumber <= 0 ? disabledClass : enabledClass);
    next.setAttribute("class", (pageNumber + 1) * pageSize >= data.length - 1 ? disabledClass : enabledClass);
  };

  var loadDataFromPagedTable = function() {
    var sourceElems = [].slice.call(pagedTable.children).filter(function(e) {
      return e.hasAttribute("data-pagedtable-source");
    });

    if (sourceElems === null || sourceElems.length !== 1) {
      throw("A single data-pagedtable-source was not found");
    }

    var source = JSON.parse(sourceElems[0].innerHTML);
    data = source.data;
    types = source.types;
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

    var infoDiv = document.createElement("div");
    infoDiv.setAttribute("class", "pagedtable-info");
    tableDiv.appendChild(infoDiv);

    var footerDiv = document.createElement("div");
    footerDiv.setAttribute("class", "pagedtable-footer");
    tableDiv.appendChild(footerDiv);

    renderHeader();
    renderBody();
    renderFooter();
  };

  var init = function() {
    loadDataFromPagedTable(pagedTable);
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
