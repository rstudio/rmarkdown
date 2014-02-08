#' Convert a document with pandoc
#'
#' Convert documents to and from various formats using the pandoc utility.
#'
#' @param input Input file (must be UTF-8 encoded)
#' @param to Format to convert to
#' @param from Format to convert from (if not specified then the format is
#'   determined based on the file extension of \code{input}).
#' @param output Output file (if not specified then determined based on format
#'   being converted to)
#' @param citeproc \code{TRUE} to run the pandoc-citeproc filter (for processing
#'   citations) as part of the conversion
#' @param options Character vector of command line options to pass to pandoc.
#' @param verbose \code{TRUE} to show the pandoc command line which was executed
#'
#' @details Supported input and output formats are described in the
#'   \href{http://johnmacfarlane.net/pandoc/README.html}{pandoc user guide}.
#'
#'   The system path as well as the version of pandoc shipped with RStudio (if
#'   running under RStudio) are scanned for pandoc and the highest version
#'   available is used.
#'
#' @examples
#' \dontrun{
#' library(rmarkdown)
#'
#' # convert markdown to various formats
#' pandoc_convert("input.md", to = "html")
#' pandoc_convert("input.md", to = "pdf")
#'
#' # process citations
#' pandoc_convert("input.md", to = "html", citeproc = TRUE)
#'
#' # add some pandoc options
#' pandoc_convert("input.md", to="pdf, options = c("--listings"))
#' }
#'
#' @export
pandoc_convert <- function(input,
                           to,
                           from = NULL,
                           output = NULL,
                           citeproc = FALSE,
                           options = NULL,
                           verbose = FALSE) {

  # ensure we've scanned for pandoc
  find_pandoc()

  # execute within the input file's directory
  oldwd <- setwd(dirname(tools::file_path_as_absolute(input)))
  on.exit(setwd(oldwd), add = TRUE)

  # input file and formats
  args <- c(input)
  args <- c(args, "--to", to)
  if (!is.null(from))
    args <- c(args, "--from", from)

  #  output file
  if (!is.null(output))
    args <- c(args, "--output", output)

  # citeproc filter if requested
  if (citeproc)
    args <- c(args, "--filter", pandoc_citeproc())

  # additional command line options
  args <- c(args, options)

  # build the conversion command
  command <- paste(quoted(pandoc()), paste(quoted(args), collapse = " "))

  # show it in verbose mode
  if (verbose)
    cat(command, "\n")

  # run the conversion
  result <- system(command)
  if (result != 0)
    stop("pandoc document conversion failed", call. = FALSE)

  invisible(NULL)
}

#' Check whether pandoc is available
#'
#' Determine whether pandoc is currently available on the system, optionally
#' checking for a specific version or greater.
#'
#' @param version Required version of pandoc
#'
#' @return Logical indicating whether a version of pandoc is available
#'
#' @details
#'
#' The system path as well as the version of pandoc shipped with RStudio (if
#' running under RStudio) are scanned for pandoc and the highest version
#' available is used.
#'
#' @examples
#' \dontrun{
#' library(rmarkdown)
#'
#' if (pandoc_available())
#'
#'   cat("pandoc is available!\n")
#'
#' if (pandoc_available("1.12.3"))
#'   cat("requried version of pandoc is available!\n")
#' }
#' @export
pandoc_available <- function(version = NULL) {

  # ensure we've scanned for pandoc
  find_pandoc()

  # check availability
  if (!is.null(.pandoc$dir))
    if (!is.null(version))
      .pandoc$version >= version
  else
    TRUE
  else
    FALSE
}


#' Functions for generating pandoc command line arguments
#'
#' Functions that assist in creating various types of pandoc command line
#' arguments (e.g. for templates, table of contents, highlighting, and content
#' includes)
#'
#' @inheritParams includes
#'
#' @param toc \code{TRUE} to include a table of contents in the output
#' @param toc_depth Depth of headers to include in table of contents
#' @param highlight The name of a pandoc syntax highlighting theme.
#' @param default The highlighting theme to use if "default"
#'   is specified.
#'
#' @return A character vector with pandoc command line arguments
#'
#' @details Non-absolute paths for resources referenced from the
#'   \code{in_header}, \code{before_body}, and \code{after_body}
#'   parameters are resolved relative to the directory of the input document.
#'
#' @examples
#' \dontrun{
#'
#' library(rmarkdown)
#'
#' pandoc_include_args(before_body = "header.htm")
#' pandoc_include_args(before_body = "header.tex")
#'
#' pancoc_highlight_args("kate")
#'
#' pandoc_toc_args(toc = TRUE, toc_depth = 2)
#'
#' }
#' @name pandoc_args
NULL


#' @rdname pandoc_args
#' @export
pandoc_include_args <- function(in_header = NULL,
                                before_body = NULL,
                                after_body = NULL) {
  args <- c()

  for (file in in_header)
    args <- c(args, "--include-in-header", pandoc_path_arg(file))

  for (file in before_body)
    args <- c(args, "--include-before-body", pandoc_path_arg(file))

  for (file in after_body)
    args <- c(args, "--include-after-body", pandoc_path_arg(file))

  args
}

#' @rdname pandoc_args
#' @export
pandoc_highlight_args <- function(highlight, default = "tango") {

  args <- c()

  if (is.null(highlight))
    args <- c(args, "--no-highlight")
  else {
    if (identical(highlight, "default"))
      highlight <- default
    args <- c(args, "--highlight-style", highlight)
  }

  args
}

#' @rdname pandoc_args
#' @export
pandoc_toc_args <- function(toc, toc_depth = 3) {

  args <- c()

  if (toc) {
    args <- c(args, "--table-of-contents")
    args <- c(args, "--toc-depth", toc_depth)
  }

  args
}


#' Transform path for passing to pandoc
#'
#' Transform a path for passing to pandoc on the command line. Calls
#' \code{\link[base:path.expand]{path.expand}} on all platforms. On Windows,
#' transform it to a short path name if it contains spaces, and then convert
#' backslashes to forward slashes
#'
#' @param path Path to transform
#'
#' @return Transformed path that can be passed to pandoc on the command line
#'
#' @export
pandoc_path_arg <- function(path) {

  path <- path.expand(path)

  if (is_windows()) {
    if (grepl(' ', path, fixed=TRUE))
      path <- utils::shortPathName(path)
    path <- gsub("\\\\", "/", path)
  }

  path
}

reconcile_self_contained <- function(self_contained, mathjax) {
  if (identical(mathjax, "local")) {
    if (self_contained)
      warning("Local MathJax isn't compatible with self_contained ",
              "(setting self_contained to FALSE)", call. = FALSE)
    self_contained <- FALSE
  }
  self_contained
}

pandoc_mathjax_args <- function(mathjax,
                                template,
                                self_contained,
                                files_dir) {
  args <- c()

  if (!is.null(mathjax)) {

    if (identical(mathjax, "default")) {
      if (identical(template, "default"))
        mathjax <- default_mathjax()
      else
        mathjax <- NULL
    }
    else if (identical(mathjax, "local")) {
      mathjax_path <- rmarkdown_system_file("rmd/h/m")
      mathjax_path <- render_supporting_files(mathjax_path,
                                              files_dir,
                                              "mathjax-2.3.0")
      mathjax_path <- pandoc_path_arg(mathjax_path)
      mathjax <- paste(mathjax_path, "/", mathjax_config(), sep = "")
    }

    if (identical(template, "default")) {
      args <- c(args, "--mathjax")
      args <- c(args, "--variable", paste("mathjax-url:", mathjax, sep=""))
    } else if (!self_contained) {
      args <- c(args, "--mathjax")
      if (!is.null(mathjax))
        args <- c(args, mathjax)
    } else {
      warning("MathJax doesn't work with self_contained when not ",
              "using the rmarkdown \"default\" template.", call. = FALSE)
    }

  }

  args
}

pandoc_html_highlight_args <- function(highlight,
                                       template,
                                       self_contained,
                                       files_dir) {

  args <- c()

  if (is.null(highlight)) {
    args <- c(args, "--no-highlight")
  }
  else if (!identical(template, "default")) {
    if (identical(highlight, "default"))
      highlight <- "pygments"
    args <- c(args, "--highlight-style", highlight)
  }
  else {
    highlight <- match.arg(highlight, html_highlighters())
    if (highlight %in% c("default", "textmate")) {
      highlight_path <- rmarkdown_system_file("rmd/h/highlight")
      if (!self_contained)
        highlight_path <- render_supporting_files(highlight_path, files_dir)
      highlight_path <- pandoc_path_arg(highlight_path)
      args <- c(args, "--no-highlight")
      args <- c(args,
                "--variable", paste("highlightjs=", highlight_path, sep=""))
      if (identical(highlight, "textmate")) {
        args <- c(args,
                  "--variable",
                  paste("highlightjs-theme=", highlight, sep=""))
      }
    }
    else {
      args <- c(args, "--highlight-style", highlight)
    }
  }

  args
}

# Scan for a copy of pandoc and set the internal cache if it's found.
find_pandoc <- function() {

  if (is.null(.pandoc$dir)) {

    # define potential sources
    sys_pandoc <- Sys.which("pandoc")
    sources <- c(ifelse(nzchar(sys_pandoc), dirname(sys_pandoc), ""),
                 Sys.getenv("RSTUDIO_PANDOC"))
    if (!is_windows())
      sources <- c(sources, "~/opt/pandoc")

    # determine the versions of the sources
    versions <- lapply(sources, function(src) {
      if (file.exists(src))
        get_pandoc_version(src)
      else
        numeric_version("0")
    })

    # find the maximum version
    found_src <- NULL
    found_ver <- numeric_version("0")
    for (i in 1:length(sources)) {
      ver <- versions[[i]]
      if (ver > found_ver) {
        found_ver <- ver
        found_src <- sources[[i]]
      }
    }

    # did we find a version?
    if (!is.null(found_src)) {
      .pandoc$dir <- found_src
      .pandoc$version <- found_ver
    }
  }
}

# Get an S3 numeric_version for the pandoc utility at the specified path
get_pandoc_version <- function(pandoc_dir) {
  pandoc_path <- file.path(pandoc_dir, "pandoc")
  version_info <- system(paste(shQuote(pandoc_path), "--version"), intern = TRUE)
  version <- strsplit(version_info, "\n")[[1]][1]
  version <- strsplit(version, " ")[[1]][2]
  numeric_version(version)
}


# get the path to the pandoc binary
pandoc <- function() {
  find_pandoc()
  file.path(.pandoc$dir, "pandoc")
}

# get the path to the pandoc-citeproc binary
pandoc_citeproc <- function() {
  find_pandoc()
  file.path(.pandoc$dir, "pandoc-citeproc")
}

# quote args if they need it
quoted <- function(args) {
  spaces <- grepl(' ', args, fixed=TRUE)
  args[spaces] <- shQuote(args[spaces])
  args
}

# Environment used to cache the current pandoc directory and version
.pandoc <- new.env()
.pandoc$dir <- NULL
.pandoc$version <- NULL




