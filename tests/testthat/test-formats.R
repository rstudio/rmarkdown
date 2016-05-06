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
