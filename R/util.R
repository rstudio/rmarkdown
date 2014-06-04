createUniqueId <- function(bytes) {
  paste(as.hexmode(sample(256, bytes)-1), collapse="")
}

is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}

# determine the output file for a pandoc conversion
pandoc_output_file <- function(input, pandoc_options) {
  to <- pandoc_options$to
  if (!is.null(pandoc_options$ext))
    ext <- pandoc_options$ext
  else if (to %in% c("latex", "beamer"))
    ext <- ".pdf"
  else if (to %in% c("html", "html5", "s5", "slidy",
                     "slideous", "dzslides", "revealjs"))
    ext <- ".html"
  else if (grepl("^markdown", to)) {
    if (!identical(tolower(tools::file_ext(input)), "md"))
      ext <- ".md"
    else {
      ext <- paste(".", strsplit(to, "[\\+\\-]")[[1]][[1]], sep = "")
    }
  }
  else
    ext <- paste(".", to, sep = "")
  output <- paste(tools::file_path_sans_ext(input), ext, sep = "")
  basename(output)
}


rmarkdown_system_file <- function(file) {
  system.file(file, package = "rmarkdown")
}

from_rmarkdown <- function(implicit_figures = TRUE) {
  rmarkdown_format(ifelse(implicit_figures, "", "-implicit_figures"))
}

is_null_or_string <- function(text) {
  is.null(text) || (is.character(text) && (length(text) == 1))
}

read_lines_utf8 <- function(file, encoding) {

  # read the file
  lines <- readLines(file, warn = FALSE)

  # normalize encoding to iconv compatible form
  if (identical(encoding, "native.enc"))
    encoding <- ""

  # convert to utf8
  if (!identical(encoding, "UTF-8"))
    iconv(lines, from = encoding, to = "UTF-8")
  else
    lines
}

file_name_without_spaces <- function(file) {
  name <- gsub(' ', '_', basename(file), fixed = TRUE)
  dir <- dirname(file)
  if (nzchar(dir) && !identical(dir, "."))
    file.path(dir, name)
  else
    name
}

# return a string as a tempfile
as_tmpfile <- function(str) {
  if (length(str) > 0) {
    str_tmpfile <- tempfile("rmarkdown-str", fileext = ".html")
    writeLines(str, str_tmpfile)
    str_tmpfile
  } else {
    NULL
  }
}

file_with_ext <- function(file, ext) {
  paste(tools::file_path_sans_ext(file), ".", ext, sep = "")
}


file_with_meta_ext <- function(file, meta_ext, ext = tools::file_ext(file)) {
  paste(tools::file_path_sans_ext(file),
        ".", meta_ext, ".", ext, sep = "")
}

knitr_files_dir <- function(file) {
  paste(tools::file_path_sans_ext(file), "_files", sep = "")
}

knitr_cache_dir <- function(file, pandoc_to) {
  paste(tools::file_path_sans_ext(file), "_cache/", pandoc_to, "/", sep = "")
}


highlighters <- function() {
  c("default",
    "tango",
    "pygments",
    "kate",
    "monochrome",
    "espresso",
    "zenburn",
    "haddock")
}

merge_lists <- function (base_list, overlay_list, recursive = TRUE) {
  if (length(base_list) == 0)
    overlay_list
  else if (length(overlay_list) == 0)
    base_list
  else {
    merged_list <- base_list
    for (name in names(overlay_list)) {
      base <- base_list[[name]]
      overlay <- overlay_list[[name]]
      if (is.list(base) && is.list(overlay) && recursive)
        merged_list[[name]] <- merge_lists(base, overlay)
      else {
        merged_list[[name]] <- NULL
        merged_list <- append(merged_list,
                              overlay_list[which(names(overlay_list) %in% name)])
      }
    }
    merged_list
  }
}

strip_white <- function (x)
{
  if (!length(x))
    return(x)
  while (is_blank(x[1])) {
    x = x[-1]
    if (!length(x))
      return(x)
  }
  while (is_blank(x[(n <- length(x))])) {
    x = x[-n]
    if (n < 2)
      return(x)
  }
  x
}

is_blank <- function (x)
{
  if (length(x))
    all(grepl("^\\s*$", x))
  else TRUE
}

trim_trailing_ws <- function (x) {
  sub("\\s+$", "", x)
}

# given a directory and a file, return a relative path from the directory to the
# file, or the unmodified file path if the file does not appear to be in the
# directory
relative_to <- function(dir, file) {
  # ensure directory ends with a /
  if (!identical(substr(dir, nchar(dir), nchar(dir)), "/")) {
    dir <- paste(dir, "/", sep="")
  }

  # if the file is prefixed with the directory, return a relative path
  if (identical(substr(file, 1, nchar(dir)), dir))
    file <- substr(file, nchar(dir) + 1, nchar(file))

  # simplify ./
  if (identical(substr(file, 1, 2), "./"))
    file <- substr(file, 3, nchar(file))

  file
}

# Find common base directory, throw error if it doesn't exist
base_dir <- function(x) {
  abs <- vapply(x, tools::file_path_as_absolute, character(1))

  base <- unique(dirname(abs))
  if (length(base) > 1) {
    stop("Input files not all in same directory, please supply explicit wd",
      call. = FALSE)
  }

  base
}

requires_lang_env_var <- function() {
  Sys.info()['sysname'] == "Linux" && is.na(Sys.getenv("LANG", unset = NA))
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


