# Given an HTML string, a library directory, and an output directory, copies the
# HTML's dependencies as appropriate and rewrites the HTML to refer to those
# files. If CSS files are among the dependencies, performs the same operation
# on each after copying.
copy_html_resources <- function(html_str, lib_dir, output_dir) {
  resource_locator <- function(input_str, resource_copier) {
    call_resource_attrs(input_str, function(node, att, src, idx) {
      # copy the resource if needed
      res_src <- resource_copier(node, att, src, idx)
      if (is.null(res_src) || nchar(res_src) < 1)
        return(NULL)

      # if the resource is a CSS file, perform a similar rewriting of its
      # content in the library directory
      res_path <- file.path(output_dir, res_src)
      if (identical(tolower(xfun::file_ext(res_path)), "css") &&
          file.exists(res_path)) {
        css_content <- copy_resources(
          file_string(res_path), lib_dir, lib_dir, call_css_resource_attrs
        )
        write_utf8(css_content, res_path)
      }
    })
  }
  copy_resources(html_str, lib_dir, output_dir, resource_locator)
}

copy_resources <- function(input_str, lib_dir, output_dir, resource_locator) {
  # ensure the lib directory exists
  dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
  relative_lib <- normalized_relative_to(output_dir, lib_dir)

  res_replacements <- c()

  resource_copier <- function(node, att, src, idx) {
    in_file <- utils::URLdecode(src)

    # only process the file if (a) it isn't already in the library, and (b)
    # it exists on the local file system (this also excludes external
    # resources such as "http://foo/bar.png")
    if (length(in_file) &&
        substr(src, 1, nchar(lib_dir) + 1) != file.path(relative_lib, "") &&
        file.exists(in_file)) {

      # check to see if it's already in the library (by absolute path)
      res_src <- normalized_relative_to(lib_dir, in_file)
      if (same_path(res_src, in_file, mustWork = FALSE)) {
        # not inside the library, copy it there
        target_dir <- if (dirname(in_file) == ".")
          lib_dir
        else
          file.path(lib_dir, dirname(in_file))
        dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
        target_res_file <- file.path(target_dir, basename(in_file))

        # copy the file to the library
        if (!file.exists(target_res_file)) {
          file.copy(in_file, target_res_file)
        }

        if (identical(lib_dir, output_dir))
          res_src <- in_file
        else
          res_src <- file.path(relative_lib, src)
      } else {
        # inside the library, fix up the URL
        res_src <- file.path(relative_lib, res_src)
      }

      # replace the reference in the document and adjust the offset
      if (!identical(src, res_src)) {
        res_replacements <<- c(res_replacements, list(list(
          pos = idx,
          len = nchar(src),
          text = res_src)))
      }

      res_src
    }
  }

  # parse the HTML and copy the resources found
  resource_locator(input_str, resource_copier)

  # rewrite the HTML to refer to the copied resources
  if (length(res_replacements) > 0) {
    ch_pos <- 1
    output_str <- ""

    # we do all replacements in bytes for performance reasons--mark the
    # output with byte encoding temporarily
    prev_encoding <- Encoding(input_str)
    Encoding(input_str) <- "bytes"
    Encoding(output_str) <- "bytes"

    for (res_rep in seq_along(res_replacements)) {
      rep <- res_replacements[[res_rep]]

      # the text from the last replacement to the current one
      before <- substr(input_str, ch_pos, rep$pos - 1)
      ch_pos <- rep$pos + rep$len

      # the text from the current replacement to the end of the output,
      # if applicable
      after <- if (res_rep == length(res_replacements)) {
        substring(input_str, ch_pos, last = nchar(input_str, type = "bytes"))
      } else {
        ""
      }

      # compose the next segment of the output from the text between
      # replacements and the current replacement text
      output_str <- paste(output_str, before, rep$text, after,
                          sep = "")
    }
    input_str <- output_str
    Encoding(input_str) <- prev_encoding
  }

  input_str
}
