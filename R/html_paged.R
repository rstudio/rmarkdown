paged_table_html <- function(x) {
  addRowNames = .row_names_info(data, type = 1) > 0

  maxPrint <- getOption("max.print", 1000)

  # hard stop at 10K items to print to prevent pandoc from failing
  maxPrint <- if (maxPrint > 10000) 10000 else maxPrint

  data <- utils::head(x, maxPrint)

  data <- if (is.null(data)) as.data.frame(list()) else data

  columnNames <- names(data)
  columnSequence <- seq_len(ncol(data))

  columns <- lapply(
    columnSequence,
    function(columnIdx) {
      column <- data[[columnIdx]]
      baseType <- class(column)[[1]]
      tibbleType <- tibble::type_sum(column)

      list(
        label = if (!is.null(columnNames)) columnNames[[columnIdx]] else "",
        name = columnIdx,
        type = tibbleType,
        align = if (baseType == "character" || baseType == "factor") "left" else "right"
      )
    }
  )

  names(data) <- as.character(columnSequence)

  if (addRowNames) {
    columns <- c(
      list(
        list(
          label = "",
          name = "_rn_",
          type = "",
          align = "left"
        )
      ),
      columns
    )

    data$`_rn_` <- rownames(data)
  }

  columns <- unname(columns)

  is_list <- vapply(data, is.list, logical(1))
  data[is_list] <- lapply(data[is_list], function(x) {
    summary <- tibble::obj_sum(x)
    paste0("<", summary, ">")
  })

  if (length(columns) > 0) {
    first_column = data[[1]]
    if (is.numeric(first_column) && isTRUE(all(diff(first_column) == 1)))
      columns[[1]]$align <- "left"
  }

  data <- as.data.frame(
    lapply(
      data,
      function (y) {
        # escape NAs from character columns
        if (typeof(y) == "character") {
          y[y == "NA"] <- "__NA__"
        }

        y <- encodeString(format(y, digits = getOption("digits")))

        # trim spaces
        gsub("^\\s+|\\s+$", "", y)
      }
    ),
    stringsAsFactors = FALSE,
    optional = TRUE)

  rowCount <- if (is.null(getOption("rows.print"))) 10 else getOption("rows.print")

  pagedTableOptions <- list(
    columns = list(
      min = getOption("cols.min.print"),
      max = getOption("cols.print", 10)
    ),
    rows = list(
      min = rowCount,
      max = rowCount
    ),
    pages = getOption("pages.print")
  )

  pagedData <- list(
    columns = columns,
    data = if (length(data) == 0) list() else data,
    options = pagedTableOptions
  )

  paste(
    "<div data-pagedtable=\"true\">",
    "  <script data-pagedtable-source type=\"application/json\">",
    jsonlite::toJSON(pagedData),
    "  </script>",
    "</div>",
    sep = "\n"
  )
}
