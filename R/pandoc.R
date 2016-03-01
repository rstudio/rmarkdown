#' Convert a document with pandoc
#'
#' Convert documents to and from various formats using the pandoc utility.
#'
#' @param input Character vector containing paths to input files
#'   (files must be UTF-8 encoded)
#' @param to Format to convert to (if not specified, you must specify
#'   \code{output})
#' @param from Format to convert from (if not specified then the format is
#'   determined based on the file extension of \code{input}).
#' @param output Output file (if not specified then determined based on format
#'   being converted to)
#' @param citeproc \code{TRUE} to run the pandoc-citeproc filter (for processing
#'   citations) as part of the conversion
#' @param options Character vector of command line options to pass to pandoc.
#' @param verbose \code{TRUE} to show the pandoc command line which was executed
#' @param wd Working directory in which code will be executed. If not
#'   supplied, defaults to the common base directory of \code{input}
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
#' pandoc_convert("input.md", to="pdf", options = c("--listings"))
#' }
#'
#' @export
pandoc_convert <- function(input,
                           to = NULL,
                           from = NULL,
                           output = NULL,
                           citeproc = FALSE,
                           options = NULL,
                           verbose = FALSE,
                           wd = NULL) {

  # ensure we've scanned for pandoc
  find_pandoc()

  # execute in specified working directory
  if (is.null(wd)) {
    wd <- base_dir(input)
  }
  oldwd <- setwd(wd)
  on.exit(setwd(oldwd), add = TRUE)


  # input file and formats
  args <- c(input)
  if (!is.null(to))
    args <- c(args, "--to", to)
  if (!is.null(from))
    args <- c(args, "--from", from)

  #  output file
  if (!is.null(output))
    args <- c(args, "--output", output)

  # set pandoc stack size
  stack_size <- getOption("pandoc.stack.size", default = "512m")
  args <- c(c("+RTS", paste0("-K", stack_size), "-RTS"), args)

  # additional command line options
  args <- c(args, options)

  # citeproc filter if requested
  if (citeproc) {
    args <- c(args, "--filter", pandoc_citeproc())
    # --natbib/--biblatex conflicts with '--filter pandoc-citeproc'
    i <- stats::na.omit(match(c("--natbib", "--biblatex"), options))
    if (length(i)) options <- options[-i]
  }

  # build the conversion command
  command <- paste(quoted(pandoc()), paste(quoted(args), collapse = " "))

  # show it in verbose mode
  if (verbose)
    cat(command, "\n")

  # run the conversion
  with_pandoc_safe_environment({
    result <- system(command)
  })
  if (result != 0)
    stop("pandoc document conversion failed with error ", result, call. = FALSE)

  invisible(NULL)
}

#' Check pandoc availabilty and version
#'
#' Determine whether pandoc is currently available on the system (optionally
#' checking for a specific version or greater). Determine the specific version
#' of pandoc available.
#'
#' @param version Required version of pandoc
#'
#' @return \code{pandoc_available} returns a logical indicating whether the
#'   required version of pandoc is available. \code{pandoc_version} returns a
#'   \code{\link[base]{numeric_version}} with the version of pandoc found.
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
#'   cat("pandoc", as.character(pandoc_version()), "is available!\n")
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


#' @rdname pandoc_available
#' @export
pandoc_version <- function() {
  find_pandoc()
  .pandoc$version
}

#' Functions for generating pandoc command line arguments
#'
#' Functions that assist in creating various types of pandoc command line
#' arguments (e.g. for templates, table of contents, highlighting, and content
#' includes)
#'
#' @inheritParams includes
#'
#' @param name Name of template variable to set.
#' @param value Value of template variable.
#' @param toc \code{TRUE} to include a table of contents in the output.
#' @param toc_depth Depth of headers to include in table of contents.
#' @param highlight The name of a pandoc syntax highlighting theme.
#' @param latex_engine LaTeX engine for producing PDF output. Options are
#'   "pdflatex", "lualatex", and "xelatex".
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
#' pandoc_latex_engine_args("pdflatex")
#'
#' pandoc_toc_args(toc = TRUE, toc_depth = 2)
#'
#' }
#' @name pandoc_args
NULL

#' @rdname pandoc_args
#' @export
pandoc_variable_arg <- function(name, value) {
  c("--variable", paste(name, "=", value, sep = ""))
}


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
pandoc_latex_engine_args <- function(latex_engine) {
  c("--latex-engine", find_latex_engine(latex_engine))
}

find_latex_engine <- function(latex_engine) {
  # use a full path to the latex engine on OSX since the stripping
  # of the PATH environment variable by OSX 10.10 Yosemite prevents
  # pandoc from finding the engine in e.g. /usr/texbin
  if (is_osx()) {
    # resolve path if it's not already an absolute path
    if (!grepl("/", latex_engine, fixed = TRUE))
      program_path <- find_program(latex_engine)
    if (nzchar(program_path))
      latex_engine <- program_path
  }
  latex_engine
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
#' forward slashes to back slashes (as required by pandoc for some path
#' references)
#'
#' @param path Path to transform
#'
#' @return Transformed path that can be passed to pandoc on the command line
#'
#' @export
pandoc_path_arg <- function(path) {

  path <- path.expand(path)

  # remove redundant ./ prefix if present
  path <- sub('^[.]/', '', path)

  if (is_windows()) {
    i <- grep(' ', path)
    if (length(i))
      path[i] <- utils::shortPathName(path[i])
    path <- gsub('/', '\\\\', path)
  }

  path
}


#' Render a pandoc template.
#'
#' Use the pandoc templating engine to render a text file. Substitutions are
#' done using the \code{metadata} list passed to the function.
#'
#' @param metadata A named list containing metadata to pass to template.
#' @param template Path to a pandoc template.
#' @param output Path to save output.
#' @param verbose \code{TRUE} to show the pandoc command line which was
#'   executed.
#' @return (Invisibly) The path of the generated file.
#'
#' @export
pandoc_template <- function(metadata, template, output, verbose = FALSE) {

  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp))

  cat("---\n", file = tmp)
  cat(yaml::as.yaml(metadata), file = tmp, append = TRUE)
  cat("---\n", file = tmp, append = TRUE)
  cat("\n", file = tmp, append = TRUE)

  pandoc_convert(tmp, "markdown", output = output,
                 options = paste0("--template=", template),
                 verbose = verbose)

  invisible(output)
}

#' Create a self-contained HTML document using pandoc.
#'
#' Create a self-contained HTML document by base64 encoding images,
#' scripts, and stylesheets referended by the input document.
#'
#' @param input Input html file to create self-contained version of.
#' @param output Path to save output.
#'
#' @return (Invisibly) The path of the generated file.
#'
#' @export
pandoc_self_contained_html <- function(input, output) {

  # make input file path absolute
  input <- normalizePath(input)

  # ensure output file exists and make it's path absolute
  if (!file.exists(output))
    file.create(output)
  output <- normalizePath(output)

  # create a simple body-only template
  template <- tempfile(fileext = ".html")
  writeLines("$body$", template)

  # convert from markdown to html to get base64 encoding
  # (note there is no markdown in the source document but
  # we still need to do this "conversion" to get the
  # base64 encoding)
  pandoc_convert(
    input = input,
    from = "markdown",
    output = output,
    options = c(
      "--self-contained",
      "--template", template
    )
  )

  invisible(output)
}



validate_self_contained <- function(mathjax) {
  if (identical(mathjax, "local"))
    stop("Local MathJax isn't compatible with self_contained\n",
         "(you should set self_contained to FALSE)", call. = FALSE)
}

pandoc_mathjax_args <- function(mathjax,
                                template,
                                self_contained,
                                files_dir,
                                output_dir) {
  args <- c()

  if (!is.null(mathjax)) {

    if (identical(mathjax, "default")) {
      if (identical(template, "default"))
        mathjax <- default_mathjax()
      else
        mathjax <- NULL
    }
    else if (identical(mathjax, "local")) {
      mathjax_path <- pandoc_mathjax_local_path()
      mathjax_path <- render_supporting_files(mathjax_path,
                                              files_dir,
                                              "mathjax-local")
      mathjax <- paste(normalized_relative_to(output_dir, mathjax_path), "/",
                       mathjax_config(), sep = "")
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


pandoc_mathjax_local_path <- function() {

  local_path <- Sys.getenv("RMARKDOWN_MATHJAX_PATH", unset = NA)
  if (is.na(local_path)) {
    local_path <- unix_mathjax_path()
    if (is.na(local_path)) {
      stop("For mathjax = \"local\", please set the RMARKDOWN_MATHJAX_PATH ",
           "environment variable to the location of MathJax. ",
           "On Linux systems you can also install MathJax using your ",
           "system package manager.")
    } else {
      local_path
    }
  } else {
    local_path
  }
}


unix_mathjax_path <- function() {
  if (identical(.Platform$OS.type, "unix")) {
    mathjax_path <- "/usr/share/javascript/mathjax"
    if (file.exists(file.path(mathjax_path, "MathJax.js")))
      mathjax_path
    else
      NA
  } else {
    NA
  }
}


pandoc_html_navigation_args <- function(self_contained,
                                        files_dir,
                                        output_dir) {
  args <- c()
  navigation_path <- rmarkdown_system_file("rmd/h/navigation-1.0")
  if (self_contained) {
    navigation_path <- pandoc_path_arg(navigation_path)
  }
  else {
    navigation_path <- normalized_relative_to(output_dir,
                                              render_supporting_files(
                                               navigation_path, files_dir))
  }
  args <- c(args,
            "--variable", paste("navigationjs=", navigation_path, sep=""))
}

pandoc_html_highlight_args <- function(highlight,
                                       template,
                                       self_contained,
                                       files_dir,
                                       output_dir) {

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
      if (self_contained)
        highlight_path <- pandoc_path_arg(highlight_path)
      else
      {
        highlight_path <- normalized_relative_to(output_dir,
          render_supporting_files(highlight_path, files_dir))
      }
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
    sys_pandoc <- find_program("pandoc")
    sources <- c(Sys.getenv("RSTUDIO_PANDOC"),
                 ifelse(nzchar(sys_pandoc), dirname(sys_pandoc), ""))
    if (!is_windows())
      sources <- c(sources, path.expand("~/opt/pandoc"))

    # determine the versions of the sources
    versions <- lapply(sources, function(src) {
      if (dir_exists(src))
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
  if (is_windows()) pandoc_path <- paste0(pandoc_path, ".exe")
  if (!utils::file_test("-x", pandoc_path)) return(numeric_version("0"))
  with_pandoc_safe_environment({
    version_info <- system(paste(shQuote(pandoc_path), "--version"),
                           intern = TRUE)
  })
  version <- strsplit(version_info, "\n")[[1]][1]
  version <- strsplit(version, " ")[[1]][2]
  numeric_version(version)
}

# wrap a system call to pandoc so that LC_ALL is not set
# see: https://github.com/rstudio/rmarkdown/issues/31
# see: https://ghc.haskell.org/trac/ghc/ticket/7344
with_pandoc_safe_environment <- function(code) {
  lc_all <- Sys.getenv("LC_ALL", unset = NA)
  if (!is.na(lc_all)) {
    Sys.unsetenv("LC_ALL")
    on.exit(Sys.setenv(LC_ALL = lc_all), add = TRUE)
  }
  lc_ctype <- Sys.getenv("LC_CTYPE", unset = NA)
  if (!is.na(lc_ctype)) {
    Sys.unsetenv("LC_CTYPE")
    on.exit(Sys.setenv(LC_CTYPE = lc_ctype), add = TRUE)
  }
  if (Sys.info()['sysname'] == "Linux" &&
        is.na(Sys.getenv("HOME", unset = NA))) {
    stop("The 'HOME' environment variable must be set before running Pandoc.")
  }
  if (Sys.info()['sysname'] == "Linux" &&
        is.na(Sys.getenv("LANG", unset = NA))) {
    # fill in a the LANG environment variable if it doesn't exist
    Sys.setenv(LANG=detect_generic_lang())
    on.exit(Sys.unsetenv("LANG"), add = TRUE)
  }
  if (Sys.info()['sysname'] == "Linux" &&
    identical(Sys.getenv("LANG"), "en_US")) {
    Sys.setenv(LANG="en_US.UTF-8")
    on.exit(Sys.setenv(LANG="en_US"), add = TRUE)
  }
  force(code)
}

# if there is no LANG environment variable set pandoc is going to hang so
# we need to specify a "generic" lang setting. With glibc >= 2.13 you can
# specify C.UTF-8 so we prefer that. If we can't find that then we fall back
# to en_US.UTF-8.
detect_generic_lang <- function() {

  locale_util <- Sys.which("locale")

  if (nzchar(locale_util)) {
    locales <- system(paste(locale_util, "-a"), intern = TRUE)
    locales <- suppressWarnings(
        strsplit(locales, split = "\n", fixed = TRUE)
    )
    if ("C.UTF-8" %in% locales)
      return ("C.UTF-8")
  }

  # default to en_US.UTF-8
  "en_US.UTF-8"
}



# get the path to the pandoc binary
pandoc <- function() {
  find_pandoc()
  file.path(.pandoc$dir, "pandoc")
}

# get the path to the pandoc-citeproc binary
pandoc_citeproc <- function() {
  find_pandoc()
  citeproc_path = file.path(.pandoc$dir, "pandoc-citeproc")
  if (file.exists(citeproc_path))
    citeproc_path
  else
    "pandoc-citeproc"
}

# quote args if they need it
quoted <- function(args) {
  # some characters are legal in filenames but without quoting are likely to be
  # interpreted by the shell (e.g. redirection, wildcard expansion, etc.) --
  # wrap arguments containing these characters in quotes.
  shell_chars <- grepl(.shell_chars_regex, args)
  args[shell_chars] <- shQuote(args[shell_chars])
  args
}

# Environment used to cache the current pandoc directory and version
.pandoc <- new.env()
.pandoc$dir <- NULL
.pandoc$version <- NULL




