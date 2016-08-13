// Production steps of ECMA-262, Edition 5, 15.4.4.18
// Reference: http://es5.github.io/#x15.4.4.18
if (!Array.prototype.forEach) {

  Array.prototype.forEach = function(callback, thisArg) {

    var T, k;

    if (this === null) {
      throw new TypeError(' this is null or not defined');
    }

    // 1. Let O be the result of calling toObject() passing the
    // |this| value as the argument.
    var O = Object(this);

    // 2. Let lenValue be the result of calling the Get() internal
    // method of O with the argument "length".
    // 3. Let len be toUint32(lenValue).
    var len = O.length >>> 0;

    // 4. If isCallable(callback) is false, throw a TypeError exception.
    // See: http://es5.github.com/#x9.11
    if (typeof callback !== "function") {
      throw new TypeError(callback + ' is not a function');
    }

    // 5. If thisArg was supplied, let T be thisArg; else let
    // T be undefined.
    if (arguments.length > 1) {
      T = thisArg;
    }

    // 6. Let k be 0
    k = 0;

    // 7. Repeat, while k < len
    while (k < len) {

      var kValue;

      // a. Let Pk be ToString(k).
      //    This is implicit for LHS operands of the in operator
      // b. Let kPresent be the result of calling the HasProperty
      //    internal method of O with argument Pk.
      //    This step can be combined with c
      // c. If kPresent is true, then
      if (k in O) {

        // i. Let kValue be the result of calling the Get internal
        // method of O with argument Pk.
        kValue = O[k];

        // ii. Call the Call internal method of callback with T as
        // the this value and argument list containing kValue, k, and O.
        callback.call(T, kValue, k, O);
      }
      // d. Increase k by 1.
      k++;
    }
    // 8. return undefined
  };
}

// Production steps of ECMA-262, Edition 5, 15.4.4.19
// Reference: http://es5.github.io/#x15.4.4.19
if (!Array.prototype.map) {

  Array.prototype.map = function(callback, thisArg) {

    var T, A, k;

    if (this == null) {
      throw new TypeError(' this is null or not defined');
    }

    // 1. Let O be the result of calling ToObject passing the |this|
    //    value as the argument.
    var O = Object(this);

    // 2. Let lenValue be the result of calling the Get internal
    //    method of O with the argument "length".
    // 3. Let len be ToUint32(lenValue).
    var len = O.length >>> 0;

    // 4. If IsCallable(callback) is false, throw a TypeError exception.
    // See: http://es5.github.com/#x9.11
    if (typeof callback !== 'function') {
      throw new TypeError(callback + ' is not a function');
    }

    // 5. If thisArg was supplied, let T be thisArg; else let T be undefined.
    if (arguments.length > 1) {
      T = thisArg;
    }

    // 6. Let A be a new array created as if by the expression new Array(len)
    //    where Array is the standard built-in constructor with that name and
    //    len is the value of len.
    A = new Array(len);

    // 7. Let k be 0
    k = 0;

    // 8. Repeat, while k < len
    while (k < len) {

      var kValue, mappedValue;

      // a. Let Pk be ToString(k).
      //   This is implicit for LHS operands of the in operator
      // b. Let kPresent be the result of calling the HasProperty internal
      //    method of O with argument Pk.
      //   This step can be combined with c
      // c. If kPresent is true, then
      if (k in O) {

        // i. Let kValue be the result of calling the Get internal
        //    method of O with argument Pk.
        kValue = O[k];

        // ii. Let mappedValue be the result of calling the Call internal
        //     method of callback with T as the this value and argument
        //     list containing kValue, k, and O.
        mappedValue = callback.call(T, kValue, k, O);

        // iii. Call the DefineOwnProperty internal method of A with arguments
        // Pk, Property Descriptor
        // { Value: mappedValue,
        //   Writable: true,
        //   Enumerable: true,
        //   Configurable: true },
        // and false.

        // In browsers that support Object.defineProperty, use the following:
        // Object.defineProperty(A, k, {
        //   value: mappedValue,
        //   writable: true,
        //   enumerable: true,
        //   configurable: true
        // });

        // For best browser support, use the following:
        A[k] = mappedValue;
      }
      // d. Increase k by 1.
      k++;
    }

    // 9. return A
    return A;
  };
}

var PagedTable = function (pagedTable) {
  var me = this;

  var source = function(pagedTable) {
    var sourceElems = [].slice.call(pagedTable.children).filter(function(e) {
      return e.hasAttribute("data-pagedtable-source");
    });

    if (sourceElems === null || sourceElems.length !== 1) {
      throw("A single data-pagedtable-source was not found");
    }

    return JSON.parse(sourceElems[0].innerHTML);
  }(pagedTable);

  var Page = function(data) {
    var me = this;

    var defaults = {
      size: 10,
      visible: 10
    };

    me.size = 10;
    me.number = 0;
    me.rows = 10;

    var getPageCount = function() {
      return Math.ceil(data.length / me.size);
    };

    me.setPageNumber = function(newPageNumber) {
      if (newPageNumber < 0) newPageNumber = 0;
      if (newPageNumber >= getPageCount()) newPageNumber = getPageCount() - 1;

      me.number = newPageNumber;
    };

    me.setVisiblePages = function(visiblePages) {
      me.visible = Math.min(defaults.visible, visiblePages);
      me.setPageNumber(me.number);
    };

    me.getVisiblePageRange = function() {
      var start = me.number - Math.max(Math.floor((me.visible - 1) / 2), 0);
      var end = me.number + Math.floor(me.visible / 2);
      var pageCount = getPageCount();

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

    me.getRowStart = function() {
      var rowStart = page.number * page.size;
      if (rowStart + me.size > data.length)
        rowStart = rowStart - (rowStart + me.size - data.length);
      if (rowStart < 0)
        rowStart = 0;

      return rowStart;
    };

    me.getRowEnd = function() {
      var rowStart = me.getRowStart();
      return Math.min(rowStart + me.size, data.length);
    };
  };

  var Columns = function(source) {
    var me = this;

    me.defaults = {
      visible: 10
    };

    me.number = 0;
    me.visible = me.defaults.visible;
    me.total = source.columns.length;
    me.subset = [];
    me.padding = 0;
    me.min = 5;

    me.updateSlice = function() {
      if (me.number + me.visible >= me.total)
        me.number = me.total - me.visible;

      if (me.number < 0) me.number = 0;

      me.subset = source.columns.slice(me.number, Math.min(me.number + me.visible, me.total));

      me.subset = me.subset.map(function(column) {
        Object.keys(column).forEach(function(colKey) {
          column[colKey] = column[colKey] === null ? "" : column[colKey].toString();
        });

        return column;
      });
    };

    me.setVisibleColumns = function(columnNumber, newVisibleColumns, paddingCount) {
      me.number = columnNumber;
      me.visible = newVisibleColumns;
      me.padding = paddingCount;

      me.updateSlice();
    };

    me.incColumnNumber = function(increment) {
      me.number = me.number + increment;
    };

    me.setColumnNumber = function(newNumber) {
      me.number = newNumber;
    };

    me.setPaddingCount = function(newPadding) {
      me.padding = newPadding;
    };

    me.getPaddingCount = function() {
      return me.padding;
    };

    me.updateSlice(0);
    return me;
  };

  var data = source.data;
  var page = new Page(data);
  var columns = new Columns(source);

  var table;
  var tableDiv;
  var header;
  var tableDivLastWidth = -1;

  var onChangeCallbacks = [];

  var clearSelection = function() {
    if(document.selection && document.selection.empty) {
      document.selection.empty();
    } else if(window.getSelection) {
      var sel = window.getSelection();
      sel.removeAllRanges();
    }
  };

  var renderColumnNavigation = function(increment, backwards) {
    var arrow = document.createElement("div");
    arrow.setAttribute("style",
      "border-top: 5px solid transparent;" +
      "border-bottom: 5px solid transparent;" +
      "border-" + (backwards ? "right" : "left") + ": 5px solid;");

    var header = document.createElement("th");
    header.appendChild(arrow);
    header.setAttribute("style",
      "cursor: pointer;" +
      "vertical-align: middle;" +
      "width: 5px;");

    header.onclick = function() {
      columns.incColumnNumber(backwards ? -1 : increment);

      me.fitColumns(backwards);

      clearSelection();
      triggerOnChange();
    };

    return header;
  };

  var renderHeader = function() {
    var thead = pagedTable.querySelectorAll("thead")[0];
    thead.innerHTML = "";

    header = document.createElement("tr");
    thead.appendChild(header);

    if (columns.number > 0)
      header.appendChild(renderColumnNavigation(-columns.visible, true));

    columns.subset = columns.subset.map(function(columnData) {
      var column = document.createElement("th");
      column.setAttribute("align", columnData.align);

      if (columnData.width) {
        column.style.minWidth =
          column.style.maxWidth = columnData.width;
      }

      var columnName = document.createElement("div");
      if (columnData.label === "") {
        columnName.innerHTML = "&nbsp;";
      }
      else {
        columnName.appendChild(document.createTextNode(columnData.label));
      }
      column.appendChild(columnName);

      var columnType = document.createElement("div");
      columnType.setAttribute("class", "pagedtable-header-type");
      if (columnData.type === "") {
        columnType.innerHTML = "&nbsp;";
      }
      else {
        columnType.appendChild(document.createTextNode("<" + columnData.type + ">"));
      }
      column.appendChild(columnType);

      header.appendChild(column);

      columnData.element = column;

      return columnData;
    });

    for (var idx = 0; idx < columns.getPaddingCount(); idx++) {
      var paddingCol = document.createElement("th");
      paddingCol.setAttribute("class", "pagedtable-padding-col");
      header.appendChild(paddingCol);
    }

    if (columns.number + columns.visible < columns.total)
      header.appendChild(renderColumnNavigation(columns.visible, false));

    return thead;
  };

  me.onChange = function(callback) {
    onChangeCallbacks.push(callback);
  };

  var triggerOnChange = function() {
    onChangeCallbacks.forEach(function(onChange) {
      onChange();
    });
  };

  var renderBody = function() {
    var tbody = pagedTable.querySelectorAll("tbody")[0];
    tbody.innerHTML = "";

    var pageData = data.slice(page.getRowStart(), page.getRowEnd());

    pageData.forEach(function(dataRow, idxRow) {
      var htmlRow = document.createElement("tr");
      htmlRow.setAttribute("class", (idxRow % 2 !==0) ? "even" : "odd");

      if (columns.number > 0)
        htmlRow.appendChild(document.createElement("td"));

      columns.subset.forEach(function(columnData) {
        var cellName = columnData.name;
        var dataCell = dataRow[cellName];
        var htmlCell = document.createElement("td");
        htmlCell.appendChild(document.createTextNode(dataCell));
        htmlCell.setAttribute("align", columnData.align);
        if (columnData.width) {
          htmlCell.style.minWidth = htmlCell.style.maxWidth = columnData.width;
        }
        htmlRow.appendChild(htmlCell);
      });

      for (var idx = 0; idx < columns.getPaddingCount(); idx++) {
        var paddingCol = document.createElement("td");
        paddingCol.setAttribute("class", "pagedtable-padding-col");
        htmlRow.appendChild(paddingCol);
      }

      if (columns.number + columns.visible < columns.total)
        htmlRow.appendChild(document.createElement("td"));

      tbody.appendChild(htmlRow);
    });

    return tbody;
  };

  var getLabelInfo = function(long) {
    var pageStart = page.getRowStart();
    var pageEnd = page.getRowEnd();
    var totalRecods = data.length;

    var infoText = (pageStart + 1) + "-" + pageEnd + " of " + totalRecods;
    if (totalRecods < page.size) {
      infoText = totalRecods + " Record" + (totalRecods != 1 ? "s" : "");
    }
    if (columns.total > columns.visible) {
      infoText = infoText + (long ? " Rows" : "") + " | " + (columns.number + 1) + "-" +
        (Math.min(columns.number + columns.visible, columns.total)) +
        " of " + columns.total + (long ? " Columns" : "");
    }

    return infoText;
  };

  var renderFooter = function() {
    var footer = pagedTable.querySelectorAll("div.pagedtable-footer")[0];
    footer.innerHTML = "";

    var next = document.createElement("a");
    next.appendChild(document.createTextNode("Next"));
    next.onclick = function() {
      page.setPageNumber(page.number + 1);
      renderBody();
      renderFooter();

      triggerOnChange();
    };
    if (data.length > page.size) footer.appendChild(next);

    var pageNumbers = document.createElement("div");
    pageNumbers.setAttribute("class", "pagedtable-indexes");

    var pageRange = page.getVisiblePageRange();
    for (var idxPage = pageRange.start; idxPage < pageRange.end; idxPage++) {
      var pageLink = document.createElement("a");
      pageLinkClass = idxPage === page.number ? "pagedtable-index pagedtable-index-current" : "pagedtable-index";
      pageLink.setAttribute("class", pageLinkClass);
      pageLink.setAttribute("data-page-index", idxPage);
      pageLink.onclick = function() {
        page.setPageNumber(parseInt(this.getAttribute("data-page-index")));
        renderBody();
        renderFooter();

        triggerOnChange();
      };

      pageLink.appendChild(document.createTextNode(idxPage + 1));
      pageNumbers.appendChild(pageLink);
    }
    if (data.length > page.size) footer.appendChild(pageNumbers);

    var previous = document.createElement("a");
    previous.appendChild(document.createTextNode("Previous"));
    previous.onclick = function() {
      page.setPageNumber(page.number - 1);
      renderBody(pagedTable);
      renderFooter(pagedTable);

      triggerOnChange();
    };
    if (data.length > page.size) footer.appendChild(previous);

    var infoLabel = document.createElement("div");
    infoLabel.setAttribute("class", "pagedtable-info");
    infoLabel.setAttribute("title", getLabelInfo(true));
    infoLabel.appendChild(document.createTextNode(getLabelInfo(false)));
    footer.appendChild(infoLabel);

    var enabledClass = "pagedtable-index-nav";
    var disabledClass = "pagedtable-index-nav pagedtable-index-nav-disabled";
    previous.setAttribute("class", page.number <= 0 ? disabledClass : enabledClass);
    next.setAttribute("class", (page.number + 1) * page.size >= data.length ? disabledClass : enabledClass);
  };

  me.render = function() {
    tableDiv = document.createElement("div");
    pagedTable.appendChild(tableDiv);
    var pagedTableClass = (data.length > 0) ? "pagedtable pagedtable-not-empty" : "pagedtable pagedtable-empty";
    tableDiv.setAttribute("class", pagedTableClass);

    table = document.createElement("table");
    table.setAttribute("cellspacing", "0");
    table.setAttribute("class", "table table-condensed");
    tableDiv.appendChild(table);

    table.appendChild(document.createElement("thead"));
    table.appendChild(document.createElement("tbody"));

    var footerDiv = document.createElement("div");
    footerDiv.setAttribute("class", "pagedtable-footer");
    tableDiv.appendChild(footerDiv);

    // if the host has not yet provided horizontal space, render hidden
    if (tableDiv.clientWidth <= 0) {
      tableDiv.style.visibility = "hidden";
    }

    me.fitColumns(false);

    // retry seizing columns later if the host has not provided space
    var retries = 20;
    function retryFitColumns() {
      retries = retries - 1;
      if (tableDiv.clientWidth <= 0 && retries > 0) {
        setTimeout(retryFitColumns, 100);
      } else {
        me.fitColumns(false);
        tableDiv.style.visibility = "visible";
        triggerOnChange();
      }
    }
    if (tableDiv.clientWidth <= 0) {
      retryFitColumns();
    }
  };

  var registerWidths = function() {
    columns.subset = columns.subset.map(function(column) {
      if (column.element.clientWidth > 0) {
        var elementStyle = window.getComputedStyle(column.element, null);
        var columnPadding = parseFloat(elementStyle.paddingLeft) +
          parseFloat(elementStyle.paddingRight);

        column.width =
          column.element.style.minWidth =
          column.element.style.maxWidth = (column.element.clientWidth - columnPadding) + "px";
      }

      return column;
    });
  };

  // The goal of this function is to add as many columns as possible
  // starting from left-to-right, when the right most limit is reached
  // it tries to add columns from the left as well.
  //
  // When startBackwards is true columns are added from right-to-left
  me.fitColumns = function(startBackwards) {
    var visibleColumns = 1;
    var columnNumber = columns.number;
    var paddingCount = 0;

    // track a list of added columns as we build the visible ones to allow us
    // to remove columns when they don't fit anymore.
    var columnHistory = [];

    var lastTableHeight = 0;
    var backwards = startBackwards;

    var tableDivStyle = window.getComputedStyle(tableDiv, null);
    var tableDivPadding = parseFloat(tableDivStyle.paddingLeft) +
      parseFloat(tableDivStyle.paddingRight);

    while (true) {
      columnHistory.push({
        columnNumber: columnNumber,
        visibleColumns: visibleColumns,
        paddingCount: paddingCount
      });

      columns.setVisibleColumns(columnNumber, visibleColumns, paddingCount);

      renderHeader();
      renderBody();
      renderFooter();

      if (tableDiv.clientWidth - tableDivPadding < table.offsetWidth) {
        break;
      }

      if (lastTableHeight > 0 &&
          lastTableHeight < table.offsetHeight) {
        break;
      }

      // if we run out of right-columns
      if (!backwards && columnNumber + columns.visible > Math.max(columns.total, columns.min)) {
        // if we started adding right-columns, try adding left-columns
        if (!startBackwards) {
          backwards = true;
        }
        else {
          break;
        }
      }

      // if we run out of left-columns
      if (backwards && columnNumber < 0) {
        // if we started adding left-columns, try adding right-columns
        if (startBackwards) {
          backwards = false;
        }
        else {
          break;
        }
      }

      // if we run out of columns to add try adding padding columns
      if (!backwards && columnNumber + columns.visible > columns.total) {
        paddingCount = paddingCount + 1;
        columns.setPaddingCount(paddingCount);
      }

      // when moving backwards try fetting left olumns first
      if (backwards) {
        columnNumber = columnNumber - 1;
      }

      visibleColumns = visibleColumns + 1;
      lastTableHeight = table.offsetHeight;
    }

    // Not everything gets rendered realtime while building a table,
    // some browsers, for isntance, don't apply font-weight until later.
    // Therefore, we need to account for this and remove at least one
    // column to leave space for this issues.
    var removeVisibleCols = columnHistory.length > 1 ? 2 : 1;
    var lastRenderableColumn = columnHistory[columnHistory.length - removeVisibleCols];

    columns.setVisibleColumns(
      lastRenderableColumn.columnNumber,
      lastRenderableColumn.visibleColumns,
      lastRenderableColumn.paddingCount);

    page.setVisiblePages(Math.max(Math.floor(pagedTable.offsetWidth / 80), 2));

    renderHeader();
    renderBody();
    renderFooter();

    registerWidths();

    tableDivLastWidth = tableDiv.clientWidth
  };

  me.resizeColumns = function() {
    var tableDivLastResizeWidth = -1;

    function resizeColumnsDelayed() {
      if (tableDiv.clientWidth !== tableDivLastResizeWidth) {
        tableDivLastResizeWidth = tableDiv.clientWidth;
        setTimeout(resizeColumnsDelayed, 500);
      } else {
        if (tableDiv.clientWidth !== tableDivLastWidth) {
          me.fitColumns(false);
          triggerOnChange();
        }
      }
    }

    resizeColumnsDelayed();
  }
};

var PagedTableDoc;
(function (PagedTableDoc) {
  var allPagedTables = [];

  PagedTableDoc.renderAll = function() {
    allPagedTables = [];

    var pagedTables = [].slice.call(document.querySelectorAll('[data-pagedtable]'));
    pagedTables.forEach(function(pagedTable, idx) {
      pagedTable.setAttribute("pagedtable-page", 0);
      pagedTable.setAttribute("class", "pagedtable-wrapper");

      var pagedTableInstance = new PagedTable(pagedTable);
      pagedTableInstance.render();

      allPagedTables.push(pagedTableInstance);
    });
  };

  PagedTableDoc.resizeAll = function() {
    allPagedTables.forEach(function(pagedTable) {
      pagedTable.fitColumns(false);
    });
  };

  window.addEventListener("resize", PagedTableDoc.resizeAll);

  return PagedTableDoc;
})(PagedTableDoc || (PagedTableDoc = {}));

window.onload = function() {
  PagedTableDoc.renderAll();
};
