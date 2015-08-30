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

from_rmarkdown <- function(implicit_figures = TRUE, extensions = NULL) {

  # paste extensions together and remove whitespace
  extensions <- paste0(extensions, collapse = "")
  extensions <- gsub(" ", "", extensions)
  
  # exclude implicit figures unless the user has added them back
  if (!implicit_figures && !grepl("implicit_figures", extensions))
    extensions <- paste0("-implicit_figures", extensions)
    
  rmarkdown_format(extensions)
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
    mark_utf8(lines)
}

# mark the encoding of character vectors as UTF-8
mark_utf8 <- function(x) {
  if (is.character(x)) {
    Encoding(x) <- 'UTF-8'
    return(x)
  }
  if (!is.list(x)) return(x)
  attrs <- attributes(x)
  res <- lapply(x, mark_utf8)
  attributes(res) <- attrs
  res
}

# TODO: remove this when fixed upstream https://github.com/viking/r-yaml/issues/6
yaml_load_utf8 <- function(string, ...) {
  string <- paste(string, collapse = '\n')
  mark_utf8(yaml::yaml.load(enc2utf8(string), ...))
}

yaml_load_file_utf8 <- function(input, ...) {
  yaml_load_utf8(readLines(input, encoding = 'UTF-8'), ...)
}

file_name_without_shell_chars <- function(file) {
  name <- gsub(.shell_chars_regex, '_', basename(file))
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

dir_exists <- function(x) {
  utils::file_test('-d', x)
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

# Regular expression representing characters likely to be considered special by
# the shell (require quoting/escaping)
.shell_chars_regex <- '[ <>()|\\:&;#?*]'

# Find a program within the PATH. On OSX we need to explictly call
# /usr/bin/which with a forwarded PATH since OSX Yosemite strips
# the PATH from the environment of child processes
find_program <- function(program) {
  if (Sys.info()["sysname"] == "Darwin") {
    res <- suppressWarnings({
      # Quote the path (so it can contain spaces, etc.) and escape any quotes 
      # and escapes in the path itself
      sanitized_path <- gsub("\\", "\\\\", Sys.getenv("PATH"), fixed = TRUE)      
      sanitized_path <- gsub("\"", "\\\"", sanitized_path, fixed = TRUE)
      system(paste("PATH=\"", sanitized_path, "\" /usr/bin/which ", program, sep=""),
             intern = TRUE)
    })
    if (length(res) == 0)
      ""
    else
      res
  } else {
    Sys.which(program)
  }
}

# given a string, escape the regex metacharacters it contains:
# regex metas are these,
#   . \ | ( ) [ { ^ $ * + ?
# as defined here:
#   http://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html
escape_regex_metas <- function(in_str) {
  gsub("([.\\|()[{^$+?])", "\\\\\\1", in_str)
}

