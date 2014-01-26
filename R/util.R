

# determine the output file for a pandoc conversion
pandocOutputFile <- function(input, to) {
  if (to %in% c("latex", "beamer"))
    ext <- ".pdf"
  else if (to %in% c("html", "html5", "revealjs"))
    ext <- ".html"
  else
    ext <- paste(".", to, sep = "")
  output <- paste(tools::file_path_sans_ext(input), ext, sep = "")
  basename(output)
}


pandocTemplate <- function(file) {
  system.file(file.path("templates", file), package = "rmarkdown")
}
