

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


