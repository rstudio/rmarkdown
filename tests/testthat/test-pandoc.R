# TODO: to remove when switching the package to edition 3
local_edition(3)

test_that("Converting bib file is working", {
  skip_on_cran()
  skip_if_not_pandoc("2.11") # only test with newer Pandoc citeproc
  bib_file <- test_path("resources/UTF8.bib")
  expect_snapshot_value(pandoc_citeproc_convert(bib_file, "list"), style = "deparse")
  expect_snapshot_output(pandoc_citeproc_convert(bib_file, "json"))
  expect_snapshot_output(pandoc_citeproc_convert(bib_file, "yaml"))
})
