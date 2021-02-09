
local_edition(3)

test_that("toc has correct identifier", {
  skip_on_cran() # avoid pandoc issue on CRAN
  tmp_file <- withr::local_tempfile()
  xfun::write_utf8(
    c("# 10 Section","","Sentence.","", "# Header","","Sentence "),
    tmp_file)
  res <- render(tmp_file, github_document(toc = TRUE, html_preview = FALSE))
  expect_snapshot_file(res, "github-toc.md", binary = FALSE)
})


test_that("github_document produces atx-header", {
  skip_on_cran() # avoid pandoc issue on CRAN
  tmp_file <- withr::local_tempfile()
  h <- paste0(Reduce(paste0, rep("#", 5), accumulate = TRUE), " title ", 1:5, "\n\n")
  xfun::write_utf8(h, tmp_file)
  res <- render(tmp_file, github_document(html_preview = FALSE))
  expect_snapshot_file(res, "github-atx.md", binary = FALSE)
})


