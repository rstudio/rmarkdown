context("formats")

test_that("formats successfully produce a document", {

  skip_on_cran()

  testFormat <- function(output_format) {
    output_file <- tempfile()
    render("test-formats.Rmd",
           output_format = output_format,
           output_file = output_file,
           quiet = TRUE)
    expect_true(file.exists(output_file))
    output_file
  }

  testFormat(html_document())
  testFormat(html_notebook())
  testFormat(html_fragment())
  testFormat(html_vignette())
  testFormat(ioslides_presentation())
  testFormat(slidy_presentation())
  testFormat(md_document())
  testFormat(pdf_document())
  testFormat(beamer_presentation())
  testFormat(word_document())

  if (requireNamespace("tufte", quietly = TRUE))
    testFormat(tufte_handout())
})

test_that("documents with spaces in names can be rendered", {

  skip_on_cran()

  # get path to notebook
  rmd_path <- "resources/empty.Rmd"

  # attempt to write to directory with spaces
  output_file <- "directory with spaces/r output.nb.html"
  dir.create(dirname(output_file))
  on.exit(unlink("directory with spaces", recursive = TRUE), add = TRUE)

  # generate copy with space in name
  with_spaces <- "directory with spaces/no content.Rmd"
  file.copy(rmd_path, with_spaces)

  output <- rmarkdown::render(with_spaces,
                              output_format = "html_notebook",
                              output_file = output_file,
                              quiet = TRUE)

  expect_true(file.exists(output))

})
