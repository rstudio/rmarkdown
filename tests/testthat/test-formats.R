# TODO: to remove when switching the package to edition 3
local_edition(3)

test_that("formats successfully produce a document", {

  testFormat <- function(output_format, df_print = NULL) {
    output_file <- I(tempfile())
    on.exit(unlink(output_file), add = TRUE)
    render(test_path("test-formats.Rmd"),
           output_format = output_format,
           output_file = output_file,
          # output_options = ifelse(is.null(df_print), NULL, list(df_print = df_print)),
           quiet = TRUE)
    expect_true(file.exists(output_file))
  }

  testFormat(html_document(), df_print = "kable")
  testFormat(html_notebook())
  testFormat(html_fragment(), df_print = "tibble")
  suppressWarnings(testFormat(html_vignette(), df_print = "tibble"))
  testFormat(ioslides_presentation(), df_print = "kable")
  testFormat(slidy_presentation(), df_print = "kable")
  testFormat(md_document(), df_print = "kable")
  testFormat(pdf_document(), df_print = "kable")
  testFormat(beamer_presentation(), df_print = "kable")
  testFormat(word_document(), df_print = "kable")
  suppressWarnings(testFormat(html_vignette()))

  if (requireNamespace("tufte", quietly = TRUE))
    suppressWarnings(testFormat(tufte_handout()))
})

test_that("documents with spaces in names can be rendered", {

  # get path to notebook
  rmd_path <- test_path("resources/empty.Rmd")

  # attempt to write to directory with spaces
  output_file <- test_path("directory with spaces/r output.nb.html")
  dir.create(dirname(output_file))
  on.exit(unlink(test_path("directory with spaces"), recursive = TRUE), add = TRUE)

  # generate copy with space in name
  with_spaces <- test_path("directory with spaces/no content.Rmd")
  file.copy(rmd_path, with_spaces)

  output <- rmarkdown::render(with_spaces,
                              output_format = "html_notebook",
                              output_file = output_file,
                              output_options = list(pandoc_args = c('--metadata', 'pagetitle=Test')),
                              quiet = TRUE)

  expect_true(file.exists(output))

})

test_that(
  "setting theme, highlight or fig_retina yields an error on html_vignette",
  {

  testFormat <- function(output_format) {
    output_file <- tempfile()
    expect_error(
      render(test_path("test-formats.Rmd"),
           output_format = output_format,
           output_file = output_file,
           quiet = TRUE)
    )
  }

  testFormat(html_vignette(theme = "z"))
  testFormat(html_vignette(fig_retina = 2))

})

test_that("pdf_document can correctly keep tex file if required", {
  rmd_file <- "test-formats.Rmd"
  # input in another dir
  dir.create(tmpdir <- tempfile())
  file.copy(test_path(rmd_file), tmpdir)
  texfile <- xfun::with_ext(rmd_file, "tex")
  render(file.path(tmpdir, rmd_file), pdf_document(keep_tex = FALSE),
         quiet = TRUE)
  expect_false(file.exists(file.path(tmpdir, texfile)))
  render(file.path(tmpdir, rmd_file), pdf_document(keep_tex = TRUE),
         quiet = TRUE)
  expect_true(file.exists(file.path(tmpdir, texfile)))
  unlink(tmpdir, recursive = TRUE)
})

test_that("url in css arg works HTML based format", {
  skip_if_offline()
  rmd <- local_rmd_file("---", "title: test", "---", "", "# test")
  css <- "https://raw.githubusercontent.com/rstudio/rmarkdown/master/tests/testthat/resources/styles.css"
  expect_error(render(rmd, html_document_base(css = css), quiet = TRUE), NA)
  skip_if_not_installed("bslib")
  expect_error(render(rmd, html_document_base(css = css, theme = list(version = 4)), quiet = TRUE), NA)
})

test_that("css arg works HTML based format", {
  rmd <- local_rmd_file("---", "title: test", "---", "", "# test")
  css <- withr::local_tempfile(fileext = ".css")
  xfun::write_utf8("h1{color:red;}", css)
  expect_error(render(rmd, html_document_base(css = css), quiet = TRUE), NA)
  skip_if_not_installed("bslib")
  expect_error(render(rmd, html_document_base(css = css, theme = list(version = 4)), quiet = TRUE), NA)
})

