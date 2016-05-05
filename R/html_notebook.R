html_notebook <- function(code_folding = "show",
                          highlight = "textmate",
                          ...)
{
  # use a pre-knit hook to capture the original document
  encoded_document <- NULL
  pre_knit <- function(input, ...) {
    encoded_document <<- base64enc::base64encode(input)
  }

  # post-processor to rename output file if necessary, and also
  # inject the (encoded) original document
  post_processor <- function(metadata, input_file, output_file, clean, verbose) {

    # read generated document contents
    contents <- read_lines_utf8(output_file, encoding = "UTF-8")

    # find the body end
    body_index <- tail(which(contents == "</body>"), n = 1)
    if (length(body_index) == 0)
      stop("failed to find '</body>' element in generated html document")

    # inject document contents
    comment <- sprintf("<!-- rnb-document-source %s -->", encoded_document)
    contents <- insert(contents, body_index, comment)

    # write to file
    cat(contents, file = output_file, sep = "\n")

    # rename to .nb.html if necessary
    nb_output_file <- output_file
    if (!ends_with_bytes(output_file, ".nb.html")) {
      nb_output_file <- gsub("\\.html$", ".nb.html", output_file)
      file.rename(output_file, nb_output_file)
    }

    nb_output_file
  }

  # generate actual format
  base_format <- html_document(code_folding = code_folding,
                               highlight = highlight,
                               ...)
  rmarkdown::output_format(
    knitr = knitr_options_notebook(),
    pandoc = NULL,
    pre_knit = pre_knit,
    post_processor = post_processor,
    base_format =  base_format
  )
}

parse_notebook <- function(path, encoding = "UTF-8") {

  contents <- read_lines_utf8(path, encoding = encoding)

  re_comment  <- "^\\s*<!--\\s*rnb-([^-]+)-(begin|end)\\s*([^\\s-]+)?\\s*-->\\s*$"
  re_document <- "^\\s*<!--\\s*rnb-document-source\\s*([^\\s-]+)\\s*-->\\s*$"

  rmd_contents <- NULL
  builder <- list_builder()

  for (row in seq_along(contents)) {
    line <- contents[[row]]

    # extract document contents
    matches <- gregexpr(re_document, line, perl = TRUE)[[1]]
    if (!identical(c(matches), -1L)) {
      start <- c(attr(matches, "capture.start"))
      end   <- start + c(attr(matches, "capture.length")) - 1
      decoded <- rawToChar(base64enc::base64decode(substring(line, start, end)))
      rmd_contents <- strsplit(decoded, "\\r?\\n", perl = TRUE)[[1]]
      next
    }

    # extract information from comment
    matches <- gregexpr(re_comment, line, perl = TRUE)[[1]]
    if (identical(c(matches), -1L))
      next

    starts <- c(attr(matches, "capture.start"))
    ends   <- starts + c(attr(matches, "capture.length")) - 1
    strings <- substring(line, starts, ends)

    n <- length(strings)
    if (n < 2)
      stop("invalid rnb comment")

    # decode comment information and update stack
    data <- list(row = row,
                 label = strings[[1]],
                 state = strings[[2]])

    # add metadata if available
    if (n >= 3 && nzchar(strings[[3]]))
      data[["meta"]] <- base64_decode_object(strings[[3]])
    else
      data["meta"] <- list(NULL)

    # append
    builder$append(data)
  }

  annotations <- builder$data()

  # extract header content
  head_start <- grep("^\\s*<head>\\s*$",  contents, perl = TRUE)[[1]]
  head_end   <- grep("^\\s*</head>\\s*$", contents, perl = TRUE)[[1]]

  list(source = contents,
       rmd = rmd_contents,
       header = contents[head_start:head_end],
       annotations = annotations)
}

notebook_annotated_output <- function(output, label, meta = NULL) {
  before <- if (is.null(meta)) {
    sprintf("\n<!-- rnb-%s-begin -->\n", label)
  } else {
    meta <- base64_encode_object(meta)
    sprintf("\n<!-- rnb-%s-begin %s -->\n", label, meta)
  }
  after  <- sprintf("\n<!-- rnb-%s-end -->\n", label)
  paste(before, output, after, sep = "\n")
}

notebook_annotated_knitr_hook <- function(label, hook, meta = NULL) {
  force(list(label, hook, meta))
  function(x, ...) {
    output <- hook(x, ...)
    meta <- if (is.function(meta)) meta(x, output, ...)
    notebook_annotated_output(output, label, meta)
  }
}

knitr_options_notebook <- function() {

  # save original hooks (restore after we've stored requisite
  # hooks in our output format)
  saved_hooks <- get_knitr_hook_list()
  on.exit(set_knitr_hook_list(saved_hooks), add = TRUE)

  # use 'render_markdown()' to get default hooks
  knitr::render_markdown()

  # store original hooks and annotate in format
  orig_knit_hooks <- knitr::knit_hooks$get()

  # generic hooks for knitr output
  hook_names <- c("source", "chunk", "plot", "text", "output",
                 "warning", "error", "message", "error")

  meta_hooks <- list(
    source  = notebook_text_hook,
    output  = notebook_text_hook,
    warning = notebook_text_hook,
    message = notebook_text_hook,
    error   = notebook_text_hook
  )

  knit_hooks <- lapply(hook_names, function(hook_name) {
    notebook_annotated_knitr_hook(hook_name,
                                  orig_knit_hooks[[hook_name]],
                                  meta_hooks[[hook_name]])
  })
  names(knit_hooks) <- hook_names

  opts_chunk <- list(render = notebook_render_hook,
                     comment = NA)

  # return as knitr options
  rmarkdown::knitr_options(knit_hooks = knit_hooks,
                           opts_chunk = opts_chunk)
}

notebook_text_hook <- function(input, output, ...) {
  list(data = input)
}

notebook_render_hook <- function(x, ...) {
  output <- knitr::knit_print(x, ...)
  if (inherits(x, "htmlwidget"))
    return(notebook_render_html_widget(output))
  output
}
