test_that("toc has correct identifier", {
  skip_on_cran() # avoid pandoc issue on CRAN
  tmp_file <- local_rmd_file(
    c("# 10 Section","","Sentence.","", "# Header","","Sentence ")
  )
  res <- render(tmp_file, github_document(toc = TRUE, html_preview = FALSE), quiet = TRUE)
  expect_snapshot_file(res, "github-toc.md", compare = compare_file_text, variant = "pandoc-3")
})

test_that("toc has correct identifier also when sections are numbered ", {
  skip_on_cran() # avoid pandoc issue on CRAN
  tmp_file <- local_rmd_file(
    c("# Section","","Sentence.","", "# Header","","Sentence ")
  )
  res <- render(tmp_file, github_document(toc = TRUE, number_sections = TRUE, html_preview = FALSE), quiet = TRUE)
  expect_snapshot_file(res, "github-toc-numbered.md", compare = compare_file_text, variant = "pandoc-3")
})


test_that("github_document produces atx-header", {
  skip_on_cran() # avoid pandoc issue on CRAN
  h <- paste0(Reduce(paste0, rep("#", 5), accumulate = TRUE), " title ", 1:5, "\n\n")
  tmp_file <- local_rmd_file(h)
  res <- render(tmp_file, github_document(html_preview = FALSE), quiet = TRUE)
  expect_snapshot_file(res, "github-atx.md", compare = compare_file_text)
})

test_that("github_document supports native math", {
  skip_on_cran()
  tmp_file <- local_rmd_file(c(
    "**The Cauchy-Schwarz Inequality**",
    "$$\\sum_{k=1}^n$$",
    "",
    "This sentence uses `$` delimiters to show math inline:  $\\sqrt{3x-1}+(1+x)^2$"
  ))
  res <- .render_and_read(tmp_file, output_format = github_document(html_preview = FALSE))
  expect_match(res, "$$\\sum_{k=1}^n$$", fixed = TRUE, all = FALSE)
  expect_match(res, "$\\sqrt{3x-1}+(1+x)^2$", fixed = TRUE, all = FALSE)
})

test_that("github_document supports math with webtex", {
  skip_on_cran()
  skip_if_offline("latex.codecogs.com")
  tmp_file <- local_rmd_file(c(
    "**The Cauchy-Schwarz Inequality**",
    "$$\\sum_{k=1}^n$$",
    "",
    "This sentence uses `$` delimiters to show math inline:  $\\sqrt{3x-1}+(1+x)^2$"
  ))
  res <- .render_and_read(tmp_file, output_format = github_document(html_preview = FALSE, math_method = "webtex"))
  expect_match(res, "^!\\[.*][(]https://latex[.]codecogs[.]com", all = FALSE)
})
