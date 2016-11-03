# paged_table_type_sum and paged_table_obj_sum should be replaced
# by tibble::type_sum once tibble supports R 3.0.0.
paged_table_type_sum <- function(x) {
  type_sum <- function(x)
  {
    format_sum <- switch (class(x)[[1]],
                          ordered = "ord",
                          factor = "fctr",
                          POSIXt = "dttm",
                          difftime = "time",
                          Date = date,
                          data.frame = class(x)[[1]],
                          tbl_df = "tibble",
                          NULL
    )
    if (!is.null(format_sum)) {
      format_sum
    } else if (!is.object(x)) {
      switch(typeof(x),
             logical = "lgl",
             integer = "int",
             double = "dbl",
             character = "chr",
             complex = "cplx",
             closure = "fun",
             environment = "env",
             typeof(x)
      )
    } else if (!isS4(x)) {
      paste0("S3: ", class(x)[[1]])
    } else {
      paste0("S4: ", methods::is(x)[[1]])
    }
  }

  type_sum(x)
}

paged_table_obj_sum <- function(x) {
  "%||%" <- function(x, y) {
    if(is.null(x)) y else x
  }

  big_mark <- function(x, ...) {
    mark <- if (identical(getOption("OutDec"), ",")) "." else ","
    formatC(x, big.mark = mark, ...)
  }

  dim_desc <- function(x) {
    dim <- dim(x) %||% length(x)
    format_dim <- vapply(dim, big_mark, character(1))
    format_dim[is.na(dim)] <- "??"
    paste0(format_dim, collapse = " \u00d7 ")
  }

  is_atomic <- function(x) {
    is.atomic(x) && !is.null(x)
  }

  is_vector <- function(x) {
    is_atomic(x) || is.list(x)
  }

  is_vector_s3 <- function(x) UseMethod("is_vector_s3")
  is_vector_s3.ordered <- function(x) TRUE
  is_vector_s3.factor <- function(x) TRUE
  is_vector_s3.Date <- function(x) TRUE
  is_vector_s3.POSIXct <- function(x) TRUE
  is_vector_s3.difftime <- function(x) TRUE
  is_vector_s3.data.frame <- function(x) TRUE
  is_vector_s3.default <- function(x) !is.object(x) && is_vector(x)

  size_sum <- function(x) {
    if (!is_vector_s3(x)) return("")

    paste0(" [", dim_desc(x), "]" )
  }

  switch(class(x)[[1]],
         POSIXlt = rep("POSIXlt", length(x)),
         list = vapply(x, paged_table_obj_sum, character(1L)),
         paste0(paged_table_type_sum(x), size_sum(x))
  )
}

#' @import methods
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
      tibbleType <- paged_table_type_sum(column)

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
    summary <- paged_table_obj_sum(x)
    paste0("<", summary, ">")
  })

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
    "<div data-pagedtable=\"false\">",
    "  <script data-pagedtable-source type=\"application/json\">",
    jsonlite::toJSON(pagedData),
    "  </script>",
    "</div>",
    sep = "\n"
  )
}
