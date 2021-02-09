
test_that("toc has correct identifier", {
  local_edition(3)
  skip_on_cran() # avoid pandoc issue on CRAN
  skip_if_not_pandoc("2.10.1") # changes in gfm writer break this test for earlier versions
  tmp_file <- local_rmd_file(
    c("# 10 Section","","Sentence.","", "# Header","","Sentence ")
  )
  res <- render(tmp_file, github_document(toc = TRUE, html_preview = FALSE))
  expect_snapshot_file(res, "github-toc.md", binary = FALSE)
})


test_that("github_document produces atx-header", {
  local_edition(3)
  skip_on_cran() # avoid pandoc issue on CRAN
  h <- paste0(Reduce(paste0, rep("#", 5), accumulate = TRUE), " title ", 1:5, "\n\n")
  tmp_file <- local_rmd_file(h)
  res <- render(tmp_file, github_document(html_preview = FALSE))
  expect_snapshot_file(res, "github-atx.md", binary = FALSE)
})


