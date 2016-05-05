context("notebook")

test_that("an example R Notebook document can be rendered and parsed", {

  # generate the file
  path <- "resources/r-notebook.Rmd"
  file <- tempfile(fileext = ".nb.html")
  rmarkdown::render(path, output_file = file)

  # if running interactively, try running the following code to open the
  # generated document -- in RStudio, you should see the source .Rmd opened,
  # with outputs populated in the editor view
  #
  #     file.edit(file)

  # try parsing and validating the parse format
  parsed <- parse_html_notebook(file)
  expect_identical(parsed$rmd, read_lines_utf8(path, encoding = "UTF-8"))
})
