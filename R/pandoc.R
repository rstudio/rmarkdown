#' Convert a document with pandoc
#'
#' Convert documents to and from various formats using the pandoc utility.
#'
#' Supported input and output formats are described in the
#' \href{https://pandoc.org/MANUAL.html}{pandoc user guide}.
#'
#' The system path as well as the version of pandoc shipped with RStudio (if
#' running under RStudio) are scanned for pandoc and the highest version
#' available is used.
#' @param input Character vector containing paths to input files
#'   (files must be UTF-8 encoded)
#' @param to Format to convert to (if not specified, you must specify
#'   \code{output})
#' @param from Format to convert from (if not specified then the format is
#'   determined based on the file extension of \code{input}).
#' @param output Output file (if not specified then determined based on format
#'   being converted to).
#' @param citeproc \code{TRUE} to run the pandoc-citeproc filter (for processing
#'   citations) as part of the conversion.
#' @param options Character vector of command line options to pass to pandoc.
#' @param verbose \code{TRUE} to show the pandoc command line which was executed
#' @param wd Working directory in which code will be executed. If not
#'   supplied, defaults to the common base directory of \code{input}.
#' @examples
#' \dontrun{
#' library(rmarkdown)
#'
#' # convert markdown to various formats
#' pandoc_convert("input.md", to = "html")
#' pandoc_convert("input.md", to = "latex")
#'
#' # process citations
#' pandoc_convert("input.md", to = "html", citeproc = TRUE)
#'
#' # add some pandoc options
#' pandoc_convert("input.md", to = "latex", options = c("--listings"))
#' }
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

  # evaluate path arguments before changing working directory
  force(output)

  # execute in specified working directory
  if (is.null(wd)) {
    wd <- base_dir(input)
  }

  oldwd <- setwd(wd)
  on.exit(setwd(oldwd), add = TRUE)

  # input file and formats
  args <- c(input)
  if (!is.null(to)) {
    if (to == 'html') to <- 'html4'
    if (to == 'pdf') to <- 'latex'
    args <- c(args, "--to", to)
  }
  if (!is.null(from))
    args <- c(args, "--from", from)

  # output file
  if (!is.null(output))
    args <- c(args, "--output", output)

  # set pandoc stack size
  args <- prepend_pandoc_stack_size(args)

  # additional command line options
  args <- c(args, options)

  # citeproc filter if requested
  if (citeproc) {
    # --natbib/--biblatex conflicts with pandoc own citation processing,
    # '--filter pandoc-citeproc' or '--citeproc'
    args <- c(args[!args %in% c("--natbib", "--biblatex")],
              pandoc_citeproc_args())
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
    stop2("pandoc document conversion failed with error ", result)

  invisible(NULL)
}


#' Convert a bibliograpy file
#'
#' Convert a bibliography file (e.g. a BibTeX file) to an R list, JSON text,
#'   or YAML text
#'
#' @param file Bibliography file
#' @param type Conversion type
#'
#' @return For `type = "list"`, and R list. For `type = "json"` or `type = "yaml"`,
#'   a character vector with the specified format.
#'
#' @export
pandoc_citeproc_convert <- function(file, type = c("list", "json", "yaml")) {

  # ensure we've scanned for pandoc
  find_pandoc()

  # resolve type
  type <- match.arg(type)

  if (pandoc_available("2.11")) {
    bin <- pandoc()
    to <- switch(type,
                 list = "csljson",
                 json = "csljson",
                 yaml = "markdown"
    )
    args <- c(file, "-s", "-t", to)
  } else {
    bin <- pandoc_citeproc()
    conversion <- switch(type,
                         list = "--bib2json",
                         json = "--bib2json",
                         yaml = "--bib2yaml"
    )
    args <- c(conversion, file)
  }

  # build the conversion command
  command <- paste(quoted(bin), paste(quoted(args), collapse = " "))

  # run the conversion
  with_pandoc_safe_environment({
    result <- system(command, intern = TRUE)
  })
  status <- attr(result, "status")
  if (!is.null(status)) {
    cat(result, sep = "\n")
    stop("Error ", status, " occurred building shared library.")
  }

  Encoding(result) <- "UTF-8"

  # convert the output if requested
  if (type == "list") {
    jsonlite::fromJSON(result, simplifyVector = FALSE)
  } else {
    result
  }
}

#' Check pandoc availability and version
#'
#' Determine whether pandoc is currently available on the system (optionally
#' checking for a specific version or greater). Determine the specific version
#' of pandoc available.
#'
#' The system environment variable \samp{PATH} as well as the version of pandoc
#' shipped with RStudio (its location is set via the environment variable
#' \samp{RSTUDIO_PANDOC} by RStudio products like the RStudio IDE, RStudio
#' Server, Shiny Server, and RStudio Connect, etc) are scanned for pandoc and
#' the highest version available is used. Please do not modify the environment
#' variable \samp{RSTUDIO_PANDOC} unless you know what it means.
#' @param version Required version of pandoc
#' @param error Whether to signal an error if pandoc with the required version
#'   is not found
#' @return \code{pandoc_available} returns a logical indicating whether the
#'   required version of pandoc is available. \code{pandoc_version} returns a
#'   \code{\link[base]{numeric_version}} with the version of pandoc found.
#' @examples
#' \dontrun{
#' library(rmarkdown)
#'
#' if (pandoc_available())
#'   cat("pandoc", as.character(pandoc_version()), "is available!\n")
#'
#' if (pandoc_available("1.12.3"))
#'   cat("required version of pandoc is available!\n")
#' }
#' @export
pandoc_available <- function(version = NULL,
                             error = FALSE) {

  # ensure we've scanned for pandoc
  find_pandoc()

  # check availability
  found <- !is.null(.pandoc$dir) && (is.null(version) || .pandoc$version >= version)

  msg <- c(
    "pandoc", if (!is.null(version)) c("version", version, "or higher"),
    "is required and was not found (see the help page ?rmarkdown::pandoc_available)."
  )
  if (error && !found) stop2(paste(msg, collapse = " "))

  found
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
#' includes).
#'
#' Non-absolute paths for resources referenced from the
#' \code{in_header}, \code{before_body}, and \code{after_body}
#' parameters are resolved relative to the directory of the input document.
#' @inheritParams includes
#' @param name Name of template variable to set.
#' @param value Value of template variable (defaults to \code{true} if missing).
#' @param toc \code{TRUE} to include a table of contents in the output.
#' @param toc_depth Depth of headers to include in table of contents.
#' @param highlight The name of a pandoc syntax highlighting theme.
#' @param latex_engine LaTeX engine for producing PDF output. Options are
#'   "pdflatex", "lualatex", "xelatex", and "tectonic".
#' @param default The highlighting theme to use if "default"
#'   is specified.
#' @return A character vector with pandoc command line arguments.
#' @examples
#' \dontrun{
#' library(rmarkdown)
#'
#' pandoc_include_args(before_body = "header.htm")
#' pandoc_include_args(before_body = "header.tex")
#'
#' pandoc_highlight_args("kate")
#'
#' pandoc_latex_engine_args("pdflatex")
#'
#' pandoc_toc_args(toc = TRUE, toc_depth = 2)
#' }
#' @name pandoc_args
NULL

#' @rdname pandoc_args
#' @export
pandoc_variable_arg <- function(name,
                                value) {

  c("--variable", if (missing(value)) name else paste(name, "=", value, sep = ""))
}

#' @rdname pandoc_args
#' @export
pandoc_metadata_arg <- function(name,
                                value) {

  c("--metadata", if (missing(value)) name else paste(name, "=", value, sep = ""))
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
pandoc_highlight_args <- function(highlight,
                                  default = "tango") {

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

  c(if (pandoc2.0()) "--pdf-engine" else "--latex-engine",
    find_latex_engine(latex_engine))
}

# For macOS, use a full path to the latex engine since the stripping
# of the PATH environment variable by OSX 10.10 Yosemite prevents
# pandoc from finding the engine in e.g. /usr/texbin
find_latex_engine <- function(latex_engine) {

  # do not need full path if latex_engine is available from PATH
  if (!is_osx() || nzchar(Sys.which(latex_engine))) return(latex_engine)
  # resolve path if it's not already an absolute path
  if (!grepl("/", latex_engine) && nzchar(path <- find_program(latex_engine)))
    latex_engine <- path
  latex_engine
}

#' @rdname pandoc_args
#' @export
pandoc_toc_args <- function(toc,
                            toc_depth = 3) {

  args <- c()

  if (toc) {
    args <- c(args, "--table-of-contents")
    args <- c(args, "--toc-depth", toc_depth)
  }

  args
}

#' @section About Pandoc citeproc:
#' For Pandoc version before 2.11, a pandoc filter \samp{pandoc-citeproc} is
#' used. Since Pandoc 2.11, the feature is built-in and activated using
#' \samp{--citeproc} flag. \samp{pandoc_citeproc_arg} will return the correct
#' switches depending on the Pandoc version in use.
#' @rdname pandoc_args
#' @export
pandoc_citeproc_args <- function() {
  if (pandoc_available("2.11"))
    "--citeproc"
  else
    c("--filter", pandoc_citeproc())
}


#' Transform path for passing to pandoc
#'
#' Transform a path for passing to pandoc on the command line. Calls
#' \code{\link[base:path.expand]{path.expand}} on all platforms. On Windows,
#' transform it to a short path name if it contains spaces, and then convert
#' forward slashes to back slashes (as required by pandoc for some path
#' references).
#' @param path Path to transform
#' @param backslash Whether to replace forward slashes in \code{path} with
#'   backslashes on Windows.
#' @return Transformed path that can be passed to pandoc on the command line.
#' @export
pandoc_path_arg <- function(path, backslash = TRUE) {

  path <- path.expand(path)

  # remove redundant ./ prefix if present
  path <- sub('^[.]/', '', path)

  if (is_windows()) {
    i <- grep(' ', path)
    if (length(i))
      path[i] <- utils::shortPathName(path[i])
    if (backslash) path <- gsub('/', '\\\\', path)
  }

  path
}


#' Render a pandoc template.
#'
#' Use the pandoc templating engine to render a text file. Substitutions are
#' done using the \code{metadata} list passed to the function.
#' @param metadata A named list containing metadata to pass to template.
#' @param template Path to a pandoc template.
#' @param output Path to save output.
#' @param verbose \code{TRUE} to show the pandoc command line which was
#'   executed.
#' @return (Invisibly) The path of the generated file.
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
#' scripts, and stylesheets referred by the input document.
#' @param input Input html file to create self-contained version of.
#' @param output Path to save output.
#' @return (Invisibly) The path of the generated file.
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
  on.exit(unlink(template), add = TRUE)
  write_utf8("$body$", template)

  # convert from markdown to html to get base64 encoding
  # (note there is no markdown in the source document but
  # we still need to do this "conversion" to get the
  # base64 encoding)

  # determine from (there are bugs in pandoc < 1.17 that
  # cause markdown_strict to hang on very large script
  # elements)
  from <- if (pandoc_available("1.17"))
            "markdown_strict"
          else
            "markdown"

  # do the conversion
  pandoc_convert(
    input = input,
    from = from,
    output = output,
    options = c(
      "--self-contained",
      "--template", template
    )
  )

  invisible(output)
}


validate_self_contained <- function(math) {
  if (identical(math$engine, "mathjax") && identical(math$url, "local"))
    stop2("Local MathJax isn't compatible with self_contained\n",
         "(you should set self_contained to FALSE)")
}

pandoc_math_engines <- function() {
  c("mathjax", "mathml", "webtex", "katex", "gladtex")
}

pandoc_math_args <- function(engine, url = NULL) {

  engine <- match.arg(engine, choices = pandoc_math_engines())

  if (!is.null(url) && engine %in% c("mathml", "gladtex")) {
    stop2(sprintf("%s does not support setting a URL.", engine))
  }

  paste0(c("--", engine, if (!is.null(url)) c("=", url)), collapse = "")
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


pandoc_html_highlight_args <- function(template,
                                       highlight,
                                       highlight_downlit = FALSE) {

  # Reminder: we do not use pandoc_path_arg() for argument to --highlight-style
  # https://github.com/rstudio/rmarkdown/issues/1976

  args <- c()

  # no highlighting engine
  if (is.null(highlight)) return(pandoc_highlight_args(NULL))

  highlight <- resolve_highlight(highlight, html_highlighters())

  check_highlightjs <- function(highlight, engine) {
    if (highlight != "default" && is_highlightjs(highlight)) {
      stop(
        sprintf(c(
          "'%s' theme is for highlightjs highlighting engine ",
          "and can't be used with %s engine."), c(highlight, engine)),
        call. = FALSE
      )
    }
  }

  # downlit engine
  if (highlight_downlit) {
    check_highlightjs(highlight, "downlit")
    default <- if (pandoc2.0()) resolve_highlight("arrow") else "pygments"
    args <- c(
      pandoc_highlight_args(highlight, default = default),
      # variable used to insert some css in a Pandoc template
      pandoc_variable_arg("highlight-downlit")
    )
  } else if (identical(template, "default") && is_highlightjs(highlight)) {
    # highlightjs engine for default template only
    args <- c(pandoc_highlight_args(NULL),
              # variable used to insert some css and js
              # in the Pandoc default template
              pandoc_variable_arg("highlightjs", "1"))
  } else {
    # Pandoc engine
    check_highlightjs(highlight, "Pandoc")
    args <- pandoc_highlight_args(highlight, default = "pygments")
  }

  args
}

is_highlightjs <- function(highlight) {
  !is.null(highlight) && (highlight %in% c("default", "textmate"))
}

resolve_highlight <- function(highlight, supported = highlighters()) {
  # for backward compatibility, partial match still need to work
  i <- pmatch(highlight, supported, nomatch = 0L)
  if (i > 0L) return(supported[i])

  # Otherwise it could be a custom (built-in) .theme file
  if (!pandoc2.0()) {
    stop("Using a custom highlighting style requires Pandoc 2.0 and above",
         call. = FALSE)
  }
  custom <- list(
    # from distill
    # https://raw.githubusercontent.com/apreshill/distill/arrow/inst/rmarkdown/templates/distill_article/resources/arrow.theme
    arrow = pkg_file_highlight("arrow.theme"),
    # from distill
    # https://github.com/rstudio/distill/blob/c98d332192ff75f268ddf69bddace34e4db6d89b/inst/rmarkdown/templates/distill_article/resources/rstudio.theme
    rstudio = pkg_file_highlight("rstudio.theme")
  )
  # if not an alias use the provided custom path
  highlight <- custom[[highlight]] %||% highlight
  # Check for extension or give informative error otherwise
  if (!identical(xfun::file_ext(highlight), "theme")) {
    msg <- c(
      sprintf("`highlight` argument must be one of %s",
              knitr::combine_words(c(supported, names(custom)), and = " or ", before = "`")),
      " or a file with extension `.theme`."
    )
    stop(msg, call. = FALSE)
  }
  highlight
}

#' Find the \command{pandoc} executable
#'
#' Searches for the \command{pandoc} executable in a few places and use the
#' highest version found, unless a specific version is requested.
#' @param cache Whether to search for \command{pandoc} again if a Pandoc
#'   directory containing the \command{pandoc} executable of the expected
#'   version (if provided) has been found previously. Search again if
#'   \code{cache = FALSE}.
#' @param dir A character vector of potential directory paths under which
#'   \command{pandoc} may be found. If not provided, this function searches for
#'   \command{pandoc} from the environment variable \var{RSTUDIO_PANDOC} (the
#'   RStudio IDE will set this variable to the directory of Pandoc bundled with
#'   the IDE), the environment variable \var{PATH}, and the directory
#'   \file{~/opt/pandoc/}.
#' @param version The version of Pandoc to look for (e.g., \code{"2.9.2.1"}). If
#'   not provided, this function searches for the highest version under the
#'   potential directories.
#' @note Usually you do not need to install Pandoc if you use the RStudio IDE,
#'   because the IDE has bundled a version of Pandoc. If you have installed a
#'   version of Pandoc by yourself and want to use this version instead, you may
#'   use the \code{dir} argument of this function.
#' @return A list containing the directory and version of Pandoc (if found).
#' @export
#' @examples rmarkdown::find_pandoc()
#' rmarkdown::find_pandoc(dir = '~/Downloads/Pandoc')
#' rmarkdown::find_pandoc(version = '2.7.3')
find_pandoc <- function(cache = TRUE, dir = NULL, version = NULL) {

  if (!cache) set_pandoc_info(NULL)  # clear previously found pandoc path
  if (!is.null(.pandoc$dir) && (is.null(version) || version == .pandoc$version))
    return(as.list(.pandoc))

  # look up pandoc in potential sources unless user has supplied `dir`
  sources <- if (length(dir) == 0) c(
    Sys.getenv("RSTUDIO_PANDOC"),
    dirname(find_program("pandoc")),
    "~/opt/pandoc"
  ) else dir
  sources <- path.expand(sources)

  # determine the versions of the sources
  versions <- lapply(sources, function(src) {
    if (dir_exists(src)) get_pandoc_version(src) else numeric_version("0")
  })

  # find the maximum version
  found_src <- NULL
  found_ver <- numeric_version("0")
  for (i in seq_along(sources)) {
    ver <- versions[[i]]
    if ((!is.null(version) && ver == version) || (is.null(version) && ver > found_ver)) {
      found_ver <- ver
      found_src <- sources[[i]]
    }
  }

  set_pandoc_info(found_src, found_ver)
  as.list(.pandoc)
}

# Get an S3 numeric_version for the pandoc utility at the specified path
get_pandoc_version <- function(pandoc_dir) {
  path <- file.path(pandoc_dir, "pandoc")
  if (is_windows()) path <- paste0(path, ".exe")
  if (!utils::file_test("-x", path)) return(numeric_version("0"))
  info <- with_pandoc_safe_environment(
    system(paste(shQuote(path), "--version"), intern = TRUE)
  )
  version <- strsplit(info, "\n")[[1]][1]
  version <- strsplit(version, " ")[[1]][2]
  numeric_version(version)
}

set_pandoc_info <- function(dir, version = if (!is.null(dir)) get_pandoc_version(dir)) {
  .pandoc$dir <- dir
  .pandoc$version <- version
}

# prepend pandoc stack size arguments
prepend_pandoc_stack_size <- function(args) {
  stack_size <- getOption("pandoc.stack.size", default = "512m")
  c(c("+RTS", paste0("-K", stack_size), "-RTS"), args)
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
    Sys.setenv(LANG = detect_generic_lang())
    on.exit(Sys.unsetenv("LANG"), add = TRUE)
  }

  if (Sys.info()['sysname'] == "Linux" &&
    identical(Sys.getenv("LANG"), "en_US")) {
    Sys.setenv(LANG = "en_US.UTF-8")
    on.exit(Sys.setenv(LANG = "en_US"), add = TRUE)
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
      return("C.UTF-8")
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
  bin <- "pandoc-citeproc"
  p <- file.path(.pandoc$dir, bin)
  if (xfun::is_windows()) p <- xfun::with_ext(p, "exe")
  if (file.exists(p)) p else bin
}

#' @rdname pandoc_args
#' @param lua_files Character vector of file paths to Lua filter files. Paths
#'   will be transformed by \code{\link{pandoc_path_arg}}.
#' @export
pandoc_lua_filter_args <- function(lua_files) {
  # Lua filters was introduced in pandoc 2.0
  if (pandoc2.0()) c(rbind("--lua-filter", pandoc_path_arg(lua_files)))
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

find_pandoc_theme_variable <- function(args) {

  range <- length(args) - 1

  for (i in 1:range) {
    if (args[[i]] == "--variable" && grepl("^theme:", args[[i + 1]])) {
      return(substring(args[[i + 1]], nchar("theme:") + 1))
    }
  }

  # none found, return NULL
  NULL
}


# Environment used to cache the current pandoc directory and version
.pandoc <- new.env()
.pandoc$dir <- NULL
.pandoc$version <- NULL

pandoc2.0 <- function() {
  pandoc_available("2.0")
}

#' Get the path of the pandoc executable
#'
#' Returns the path of the pandoc executable used by functions in the the
#' \pkg{rmarkdown} package. This is the most recent version of pandoc found in
#' either the system path or shipped with RStudio.
#'
#' See the
#' \href{https://pandoc.org/MANUAL.html}{pandoc manual}
#' for pandoc commands.
#'
#' @export
pandoc_exec <- pandoc
