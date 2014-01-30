

# determine the output file for a pandoc conversion
pandoc_output_file <- function(input, to) {
  if (to %in% c("latex", "beamer"))
    ext <- ".pdf"
  else if (to %in% c("html", "html5", "revealjs"))
    ext <- ".html"
  else
    ext <- paste(".", to, sep = "")
  output <- paste(tools::file_path_sans_ext(input), ext, sep = "")
  basename(output)
}


pandoc_template <- function(file) {
  system.file(file.path("templates", file), package = "rmarkdown")
}

from_rmarkdown <- function(implicit.figures) {
  rmarkdown_format(ifelse(implicit.figures, "", "-implicit_figures"))
}

is_null_or_string <- function(text) {
  is.null(text) || (is.character(text) && (length(text) == 1))
}

highlighters <- function() {
  c("default",
    "pygments",
    "kate",
    "monochrome",
    "espresso",
    "zenburn",
    "haddock",
    "tango")
}


