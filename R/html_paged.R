paged_table_html = function(x) {
  data <- head(x, getOption("max.print", 1000))
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

  names(data) <- columnSequence

  # add the names column
  columns <- unname(
    c(
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
  )

  data$`_rn_` <- rownames(data)

  is_list <- vapply(data, is.list, logical(1))
  data[is_list] <- lapply(data[is_list], function(x) {
    summary <- tibble::obj_sum(x)
    paste0("<", summary, ">")
  })

  if (length(columns) > 0) {
    first_column = data[[1]]
    if (is.numeric(first_column) && all(diff(first_column) == 1))
      columns[[1]]$align <- "left"
  }

  data <- as.data.frame(
    lapply(
      data,
      function (y) format(y)),
    stringsAsFactors = FALSE,
    optional = TRUE)

  list(
    columns = columns,
    data = if (length(data) == 0) list() else data
  )

  paste(
    "<div data-pagedtable>",
    "  <script data-pagedtable-source type=\"application/json\">",
    jsonlite::toJSON(list(
      columns = columns,
      data = data
    )),
    "  </script>",
    "</div>",
    sep = "\n"
  )
}
