# TODO: to remove when switching the package to edition 3
local_edition(3)

test_that("toc has correct identifier", {
  skip_on_cran() # avoid pandoc issue on CRAN
  skip_if_not_pandoc("2.10.1") # changes in gfm writer break this test for earlier versions
  tmp_file <- local_rmd_file(
    c("# 10 Section","","Sentence.","", "# Header","","Sentence ")
  )
  pandoc_version <- if (pandoc_available("2.18.0.1")) {
    "after-pandoc-2.18"
  } else if (pandoc_available("2.18")) {
    "pandoc-2.18"
  } else {
    "before-pandoc-2.18"
  }
  res <- render(tmp_file, github_document(toc = TRUE, html_preview = FALSE))
  expect_snapshot_file(res, "github-toc.md", compare = compare_file_text, variant = pandoc_version)
})

test_that("toc has correct identifier also when sections are numbered ", {
  skip_on_cran() # avoid pandoc issue on CRAN
  skip_if_not_pandoc("2.10.1") # changes in gfm writer break this test for earlier versions
  tmp_file <- local_rmd_file(
    c("# Section","","Sentence.","", "# Header","","Sentence ")
  )
  pandoc_version <- if (pandoc_available("2.18.0.1")) {
    "after-pandoc-2.18"
  } else if (pandoc_available("2.18")) {
    "pandoc-2.18"
  } else {
    "before-pandoc-2.18"
  }
  res <- render(tmp_file, github_document(toc = TRUE, number_sections = TRUE, html_preview = FALSE))
  expect_snapshot_file(res, "github-toc-numbered.md", compare = compare_file_text, variant = pandoc_version)
})


test_that("github_document produces atx-header", {
  skip_on_cran() # avoid pandoc issue on CRAN
  h <- paste0(Reduce(paste0, rep("#", 5), accumulate = TRUE), " title ", 1:5, "\n\n")
  tmp_file <- local_rmd_file(h)
  res <- render(tmp_file, github_document(html_preview = FALSE))
  expect_snapshot_file(res, "github-atx.md", compare = compare_file_text)
})


