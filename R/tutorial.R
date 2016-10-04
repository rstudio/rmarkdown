
# tutorial_shiny_app is going to need to copy the files
# into a temporary directory (to avoid races on RSC when
# running in the same appDir)

# The IDE is assuming that Run Document == Reload which
# sometimes causes connection refused errors. Run Document
# should stop and restart the document

tutorial_shiny_app <- function(tutorial_rmd, encoding, render_args) {

  # render rmd
  args <- merge_lists(list(input = tutorial_rmd,
                           encoding = encoding,
                           output_options = list(self_contained = FALSE)),
                      render_args)
  tutorial_html <- do.call(render, args)

  # normalize path and get directory
  tutorial_html <- normalizePath(tutorial_html, winslash = "/")
  tutorial_dir <- dirname(tutorial_html)

  # add some resource paths
  add_resource_path <- function(path) {
    if (file_test("-d", path))
      shiny::addResourcePath(basename(path), path)
  }
  stem <- tools::file_path_sans_ext(basename(tutorial_html))
  add_resource_path(file.path(tutorial_dir,paste0(stem, "_files")))
  add_resource_path(file.path(tutorial_dir,"css"))
  add_resource_path(file.path(tutorial_dir,"js"))
  add_resource_path(file.path(tutorial_dir,"images"))
  add_resource_path(file.path(tutorial_dir,"www"))

  # read in the htm, add the shiny {{ headContent() }}, then remove
  # any other lines that include jquery.min.js (since shiny does this)
  html <- readChar(tutorial_html, file.info(tutorial_html)$size,
                   useBytes = TRUE)
  Encoding(html) <- "UTF-8"
  html <- sub("<head>", "<head>{{ headContent() }}", html)
  html <- gsub('<script src=".*jquery\\.min\\.js"></script>', '', html)

  # create shiny app
  shiny::shinyApp(
    ui = htmlTemplate(text_ = html),
    server = function(input, output, session) {

    }
  )
}
