context("notebook")

test_that("an example R Notebook document can be rendered and parsed", {

  # generate the file
  path <- test_path("resources/r-notebook.Rmd")
  file <- tempfile(fileext = ".nb.html")
  on.exit(unlink(file), add = TRUE)
  rmarkdown::render(path, output_file = file, quiet = TRUE)

  # if running interactively, try running the following code to open the
  # generated document -- in RStudio, you should see the source .Rmd opened,
  # with outputs populated in the editor view
  #
  #     file.edit(file)

  # try parsing and validating the parse format
  parsed <- parse_html_notebook(file)
  expect_identical(parsed$rmd, read_lines_utf8(path, encoding = "UTF-8"))
})

test_that("a custom output_source can be used on render", {

  # set up output_source hook
  output_options <- list(output_source = function(code, context, ...) {
    # instead of evaluating code, just return the label
    context$label
  })

  # render file
  path <- test_path("resources/r-notebook.Rmd")
  file <- tempfile(fileext = ".nb.html")
  on.exit(unlink(file), add = TRUE)
  render(path, output_options = output_options, output_file = file, quiet = TRUE)

  # parse notebook
  parsed <- parse_html_notebook(file)

  # extract metadata
  metadata <- unlist(Filter(Negate(is.null), lapply(parsed$annotations, function(annotation) {
    annotation$meta$data
  })))

  # strip whitespace
  metadata <- gsub("^\\s*|\\s*$", "", metadata)

  # check metadata
  expect_identical(metadata, c("chunk-one", "chunk-two", "chunk-three"))


})
