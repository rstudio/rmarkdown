#' Find External Resource References
#'
#' Given an R Markdown document or HTML file, attempt to determine the set of
#' additional files needed in order to render and display the document.
#'
#' This routine applies heuristics in order to scan a document for
#' possible resource references.
#'
#' In R Markdown documents, it looks for references to files implicitly
#' referenced in Markdown (e.g. \code{![alt](img.png)}), in the document's
#' YAML header, in raw HTML chunks, and as quoted strings in R code chunks
#' (e.g. \code{read.csv("data.csv")}).
#'
#' Resources specified explicitly in the YAML header for R Markdown
#' documents are also returned. To specify resources in YAML, use the
#' \code{resource_files} key:
#'
#'   \preformatted{---
#' title: My Document
#' author: My Name
#' resource_files:
#'   - data/mydata.csv
#'   - images/figure.png
#' ---}
#'
#' Each item in the \code{resource_files} list can refer to:
#' \enumerate{
#' \item A single file, such as \code{images/figure.png}, or
#' \item A directory, such as \code{resources/data}, in which case all of the
#'   directory's content will be recursively included, or
#' \item A wildcard pattern, such as \code{data/*.csv}, in which case all of
#'   the files matching the pattern will be included. No recursion is done in
#'   this case.
#' }
#'
#' In HTML files (and raw HTML chunks in R Markdown documents), this routine
#' searches for resources specified in common tag attributes, such as
#' \code{<img src="...">}, \code{<link href="...">}, etc.
#'
#' In all cases, only resources that exist on disk and are contained in the
#' document's directory (or a child thereof) are returned.
#' @param input_file path to the R Markdown document or HTML file to process
#' @inheritParams render
#' @return A data frame with the following columns:
#'   \describe{
#'    \item{path}{The relative path from the document to the resource}
#'    \item{explicit}{Whether the resource was specified explicitly
#'      (\code{TRUE}) or discovered implicitly (\code{FALSE})}
#'    \item{web}{Whether the resource is needed to display a Web page rendered
#'      from the document}
#'   }
#' @export
find_external_resources <- function(input_file, encoding = 'UTF-8') {

  # ensure we're working with valid input
  ext <- tolower(xfun::file_ext(input_file))
  if (!(ext %in% c("md", "rmd", "html", "htm", "r", "css"))) {
    stop("Resource discovery is only supported for R Markdown files or HTML files.")
  }

  if (!file.exists(input_file)) {
    stop("The input file file '", input_file, "' does not exist.")
  }

  # set up the frame we'll use to report results
  discovered_resources <- data.frame(
    path = character(0), explicit = logical(0), web = logical(0)
  )

  input_dir <- dirname(normalize_path(input_file))

  # discover a single resource--tests a string to see if it corresponds to a
  # resource on disk; if so, adds it to the list of known resources and returns
  # TRUE
  discover_single_resource <- function(path, explicit, web) {

    if (!(is.character(path) && length(path) == 1 && path != "." && path != ".." &&
          file.exists(file.path(input_dir, path))))
      return(FALSE)

    ext <- tolower(xfun::file_ext(file.path(input_dir, path)))

    if (identical(ext, "r")) {
      # if this is a .R script, look for resources it contains, too
      discover_r_resources(file.path(input_dir, path), discover_single_resource)
    } else if (identical(ext, "css")) {
      # if it's a CSS file, look for files it references (e.g. fonts/images)
      discover_css_resources(file.path(input_dir, path), discover_single_resource)
    }
    # if this is an implicitly discovered resource, it needs to refer to
    # a file rather than a directory
    if (!explicit && dir_exists(file.path(input_dir, path))) {
      return(FALSE)
    }

    # this looks valid; remember it
    discovered_resources <<- rbind(discovered_resources, data.frame(
      path = path, explicit = explicit, web = web, stringsAsFactors = FALSE
    ))
    TRUE
  }

  # run the main resource discovery appropriate to the file type
  if (ext %in% c("md", "rmd")) {
    # discover R Markdown doc resources--scans the document itself as described
    # in comments above, renders as Markdown, and invokes HTML discovery
    # on the result
    discover_rmd_resources(input_file, discover_single_resource)
  } else if (ext %in% c("htm", "html")) {
    # discover HTML resources
    discover_html_resources(input_file, discover_single_resource)

    # if the HTML file represents a rendered R Markdown document, it may have a
    # sidecar _files folder; include that if it's present
    sidecar_files_dir <- knitr_files_dir(input_file)
    files_dir_info <- file.info(sidecar_files_dir)
    if (isTRUE(files_dir_info$isdir)) {
      # we probably auto-discovered some resources from _files--exclude those
      # since they'll be covered by the directory
      files_dir_prefix <- file.path(basename(sidecar_files_dir), "")
      files_dir_matches <- substr(
        discovered_resources$path, 1, nchar(files_dir_prefix)
      ) == files_dir_prefix
      discovered_resources <- discovered_resources[!files_dir_matches, , drop = FALSE]

      # add the directory itself
      discovered_resources <- rbind(discovered_resources, data.frame(
        path = files_dir_prefix, explicit = FALSE, web = TRUE,
        stringsAsFactors = FALSE)
      )
    }
  } else if (ext == "r") {
    discover_r_resources(input_file, discover_single_resource)
  } else if (ext == "css") {
    discover_css_resources(input_file, discover_single_resource)
  }

  # clean row names (they're not meaningful)
  rownames(discovered_resources) <- NULL

  # convert paths from factors if necssary, and clean any redundant ./ leaders
  discovered_resources$path <- as.character(discovered_resources$path)
  has_prefix <- grepl("^\\./", discovered_resources$path)
  discovered_resources$path[has_prefix] <- substring(discovered_resources$path[has_prefix], 3)

  discovered_resources
}

# discovers resources in a single HTML file
discover_html_resources <- function(html_file, discover_single_resource) {
  # resource accumulator
  discover_resource <- function(node, att, val, idx) {
    res_file <- utils::URLdecode(val)
    discover_single_resource(res_file, FALSE, TRUE)
  }

  # create a single string with all of the lines in the document
  html_lines <- file_string(html_file)

  # parse the HTML and invoke our resource discovery callbacks
  call_resource_attrs(html_lines, discover_resource)
}

# discovers resources in a single R Markdown document
discover_rmd_resources <- function(rmd_file, discover_single_resource) {

  # create a UTF-8 encoded Markdown file to serve as the resource discovery
  # source
  md_file <- tempfile(fileext = ".md")
  input_dir <- dirname(normalize_path(rmd_file))
  output_dir <- dirname(md_file)
  rmd_content <- read_utf8(rmd_file)
  if (length(i <- grep('^---\\s*$', rmd_content)) >= 2 && i[1] == 1) {
    rmd_content <- append(rmd_content, 'citeproc: false', i[2] - 1)
  }
  write_utf8(rmd_content, md_file)

  # create a vector of temporary files; anything in here
  # will be cleaned up on exit
  temp_files <- md_file
  on.exit(unlink(temp_files, recursive = TRUE), add = TRUE)

  # discovers render-time resources; if any are found, adds them to the list of
  # discovered resources and copies them alongside the input document.
  discover_render_resource <- function(output_render_file) {
    if (discover_single_resource(output_render_file, FALSE, FALSE)) {
      # mirror original directory structure so we don't need to mutate input
      # prior to render
      output_target_file <- file.path(output_dir, output_render_file)
      if (!file.exists(dirname(output_target_file))) {
        dir.create(dirname(output_target_file), showWarnings = FALSE, recursive = TRUE)
      }

      # copy the original resource to the temporary render folder
      file.copy(file.path(input_dir, output_render_file), output_target_file)

      # clean up this file when we're done
      temp_files <<- c(temp_files, output_target_file)
    }
  }

  # parse the YAML front matter to discover resources named there
  front_matter <- yaml_front_matter(md_file)

  # Check for content referred to by output format calls to the includes
  # function (for generating headers/footers/etc. at render time), and for
  # references to files in pandoc arguments.
  #
  # These will be needed to produce even a vanilla Markdown variant of the input
  # document, so copy them to the temporary folder in preparation for rendering
  # (in addition to marking them as required resources).
  output_formats <- front_matter[["output"]]
  if (is.list(output_formats)) {
    for (output_format in output_formats) {
      if (is.list(output_format)) {
        output_render_files <- unlist(output_format[c(
          'includes', 'pandoc_args', 'logo', 'reference_doc', 'reference_docx', 'template'
        )])
        lapply(output_render_files, discover_render_resource)
      }
    }
  }

  # check for explicitly named resources
  if (!is.null(front_matter$resource_files)) {
    lapply(front_matter$resource_files, function(res) {
      explicit_res <- if (is.character(res)) {
        list(path = res, explicit = TRUE, web = is_web_file(res))
      } else if (is.list(res) && length(names(res)) > 0) {
        # list--happens when web flag is specified explicitly in YAML.
        list(path = names(res)[[1]],
             explicit = TRUE,
             web = if (is.null(res$web)) is_web_file(res) else res$web)
      }

      # check the extracted filename to see if it exists
      if (!is.null(explicit_res)) {
        if (grepl("*", explicit_res$path, fixed = TRUE)) {
          # if the resource file spec includes a wildcard, list the files
          # that match the pattern
          files <- list.files(
            file.path(input_dir, dirname(explicit_res$path)),
            utils::glob2rx(basename(explicit_res$path))
          )
          lapply(files, function(f) discover_single_resource(
            file.path(dirname(explicit_res$path), f), TRUE, web = is_web_file(f)
          ))
        } else {
          # no wildcard, see whether this resource refers to a directory or to
          # an individual file
          info <- file.info(file.path(input_dir, explicit_res$path))
          if (is.na(info$isdir)) {
            # implies that the file doesn't exist (should we warn here?)
            NULL
          } else if (isTRUE(info$isdir)) {
            # if the resource file spec is a directory, include all the files in
            # the directory, recursively
            files <- list.files(
              file.path(input_dir, explicit_res$path), recursive = TRUE
            )
            lapply(files, function(f) discover_single_resource(
              file.path(explicit_res$path, f), TRUE, web = is_web_file(f)
            ))
          } else {
            # isdir is false--this is an individual file; return it
            discover_single_resource(explicit_res$path, explicit_res$explicit, explicit_res$web)
          }
        }
      } else {
        discover_single_resource(explicit_res$path, explicit_res$explicit, explicit_res$web)
      }
    })
  }

  # check for a 'preview' yaml metadata entry
  if (!is.null(front_matter[["preview"]])) {
    discover_single_resource(front_matter[["preview"]], explicit = FALSE, web = TRUE)
  }

  # check for bibliography and csl files at the top level
  for (bibfile in c("bibliography", "csl")) {
    lapply(front_matter[[bibfile]], discover_render_resource)
  }

  # check for parameter values that look like files.
  if (!is.null(front_matter$params)) {
    # This is the raw parameter information and has not had any YAML tag
    # processing performed. See `knitr:::resolve_params`.
    lapply(front_matter$params, function(param) {
      if (is.list(param)) {
        if (identical(param$input, "file")) {
          if (!is.null(param$value)) {
            # We treat param filenames as non-web resources.
            discover_single_resource(param$value, TRUE, FALSE)
          }
        }
      }
    })
  }

  # check for knitr child documents in R Markdown documents
  if (tolower(xfun::file_ext(rmd_file)) == "rmd") {
    chunk_lines <- gregexpr(knitr::all_patterns$md$chunk.begin, rmd_content, perl = TRUE)
    for (idx in seq_along(chunk_lines)) {
      chunk_line <- chunk_lines[idx][[1]]
      if (is.na(chunk_line) || chunk_line < 0) next
      chunk_start <- attr(chunk_line, "capture.start", exact = TRUE) + 1
      chunk_text <- substr(
        rmd_content[idx], chunk_start,
        chunk_start + attr(chunk_line, "capture.length", exact = TRUE) - 2
      )
      for (child_expr in c("\\bchild\\s*=\\s*'([^']+)'", "\\bchild\\s*=\\s*\"([^\"]+)\"")) {
        child_match <- gregexpr(child_expr, chunk_text, perl = TRUE)[[1]]
        if (child_match > 0) {
          child_start <- attr(child_match, "capture.start", exact = TRUE)
          child_text <- substr(
            chunk_text, child_start,
            child_start + attr(child_match, "capture.length", exact = TRUE) - 1
          )
          discover_render_resource(child_text)
        }
      }
    }
  }

  # render "raw" markdown to HTML
  html_file <- tempfile(fileext = ".html")
  on.exit(unlink(html_file), add = TRUE)

  # check to see what format this document is going to render as; if it's a
  # format that produces HTML, let it render as-is, but if it isn't, render as
  # html_document to pick up dependencies
  output_format <- output_format_from_yaml_front_matter(rmd_content)

  output_format_function <- eval(xfun::parse_only(output_format$name))

  override_output_format <- if (!is_pandoc_to_html(output_format_function()$pandoc)) "html_document"

  html_file <- render(
    md_file, override_output_format, html_file, quiet = TRUE,
    output_options = list(
      self_contained = FALSE,
      pandoc_args = c("--metadata", "pagetitle=PREVIEW")
    )
  )

  # clean up output file and its supporting files directory
  temp_files <- c(temp_files, html_file, knitr_files_dir(md_file), knitr_files_dir(html_file))

  # run the HTML resource discovery mechanism on the rendered output
  discover_html_resources(html_file, discover_single_resource)

  # if this is an R Markdown file, purl the file to extract just the R code
  if (tolower(xfun::file_ext(rmd_file)) == "rmd") {
    r_file <- tempfile(fileext = ".R")
    # suppress possible try() errors https://github.com/rstudio/rmarkdown/issues/1247
    try_file <- tempfile()
    opts <- options(try.outFile = try_file)
    on.exit({
      unlink(c(r_file, try_file)); options(opts)
    }, add = TRUE)
    knitr::purl(md_file, output = r_file, quiet = TRUE, documentation = 0)
    temp_files <- c(temp_files, r_file)
    discover_r_resources(r_file, discover_single_resource)
  }
}

discover_r_resources <- function(r_file, discover_single_resource) {

  # read the lines from the R file
  r_lines <- read_utf8(r_file)

  # clean comments from the R code (simply; consider: # inside strings)
  r_lines <- sub("#.*$", "", r_lines)

  # find quoted strings in the code and attempt to ascertain whether they are
  # files on disk
  r_lines <- one_string(r_lines)
  quoted_strs <- Reduce(c, lapply(c("\"[^\"\n]*\"", "'[^'\n]*'"), function(pat) {
    matches <- unlist(regmatches(r_lines, gregexpr(pat, r_lines)))
    substr(matches, 2, nchar(matches) - 1)
  }))

  # consider any quoted string containing a valid relative path to a file that
  # exists on disk to be a reference
  for (quoted_str in quoted_strs) {
    if (nchar(quoted_str) > 0)
       discover_single_resource(quoted_str, FALSE, is_web_file(quoted_str))
  }
}

# copies the external resources needed to render original_input into
# intermediates_dir; with skip_web, skips web resources. returns a character
# vector containing paths to all resources copied.
copy_render_intermediates <- function(original_input, intermediates_dir, skip_web) {

  # start with an empty set of intermediates
  intermediates <- c()

  # extract all the resources used by the input file; note that this actually
  # runs another (non-knitting) render, and that recursion is avoided because
  # we explicitly render with self-contained = FALSE while discovering
  # resources
  resources <- find_external_resources(original_input)
  dest_dir <- normalize_path(intermediates_dir)
  source_dir <- dirname(normalize_path(original_input))

  # process each returned reosurce
  by(resources, seq_len(nrow(resources)), function(res) {
    # skip web resources if requested
    if (skip_web && res$web) return()
    dest <- copy_file_with_dir(res$path, dest_dir, source_dir)
    intermediates <<- c(intermediates, dest)
  })

  # return the list of files we generated
  intermediates
}

# copy a file from a relative path to a destination dir, and preserve its
# original dir structure, e.g., if we copy foo/bar.txt to /tmp, the destination
# file should be /tmp/foo/bar.txt instead of /tmp/bar.txt
copy_file_with_dir <- function(path, dest, from = '.') {
  dest <- file.path(dest, path)
  path <- file.path(from, path)
  if (!file.exists(path)) return()
  if (!dir_exists(dirname(dest))) dir.create(dirname(dest), recursive = TRUE)
  file.copy(path, dest)
  dest
}

discover_css_resources <- function(css_file, discover_single_resource) {

  css_lines <- read_utf8(css_file)

  discover_resource <- function(node, att, val, idx) {
    res_file <- utils::URLdecode(val)
    discover_single_resource(res_file, FALSE, TRUE)
  }

  call_css_resource_attrs(one_string(css_lines), discover_resource)
}

# given a filename, return true if the file appears to be a web file
is_web_file <- function(filename) {
  tolower(xfun::file_ext(filename)) %in% c(
    "css", "gif", "htm", "html", "jpeg", "jpg", "js", "mp3", "mp4", "png", "wav"
  )
}
