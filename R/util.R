

is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}

# determine the output file for a pandoc conversion
pandoc_output_file <- function(input, to) {
  if (to %in% c("latex", "beamer"))
    ext <- ".pdf"
  else if (to %in% c("html", "html5", "revealjs"))
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
  iconv(lines, from = encoding, to = "UTF-8")
}

file_with_meta_ext <- function(file, meta_ext, ext = tools::file_ext(file)) {
  paste(tools::file_path_sans_ext(file),
        ".", meta_ext, ".", ext, sep = "")
}

knitr_files_dir <- function(file) {
  paste(tools::file_path_sans_ext(file), "_files", sep = "")
}

knitr_cache_dir <- function(file) {
  paste(tools::file_path_sans_ext(file), "_cache", sep = "")
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

merge_lists <- function (base_list, overlay_list) {
  merged_list <- base_list
  for (name in unique(c(names(base_list), names(overlay_list)))) {
    base <- base_list[[name]]
    overlay <- overlay_list[[name]]
    if (is.null(base))
      merged_list[[name]] <- overlay
    else if (is.list(base) && is.list(overlay))
      merged_list[[name]] <- merge_lists(base, overlay)
    else if (!is.null(overlay))
      merged_list[[name]] <- overlay
  }
  merged_list
}



