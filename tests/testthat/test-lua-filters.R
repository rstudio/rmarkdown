.generate_md_and_convert <- function(content, output_format) {
  input_file <- tempfile(fileext = ".Rmd")
  output_file <- tempfile()
  on.exit(unlink(c(input_file, output_file)), add = TRUE)
  xfun::write_utf8(c("---\ntitle: Test\n---\n", content), input_file)
  res <- rmarkdown::render(input_file, output_format = output_format, output_file = output_file, quiet = TRUE)
  xfun::read_utf8(res)
}

# lua filters exists only since pandoc 2.0
skip_if_not(rmarkdown::pandoc_available("2.0"))

test_that("pagebreak lua filters works", {
  rmd <- "# HEADER 1\n\\newpage\n# HEADER 2\n\\pagebreak\n# HEADER 3"
  res <- .generate_md_and_convert(rmd, "html_document")
  expect_match(res[grep("HEADER 1", res)+1], "<div style=\"page-break-after: always;\"></div>")
  expect_match(res[grep("HEADER 2", res)+1], "<div style=\"page-break-after: always;\"></div>")
  # add a class instead of inline style
  rmd2 <- paste0("---\nnewpage_html_class: page-break\n---\n", rmd)
  res <- .generate_md_and_convert(rmd2, "html_document")
  expect_match(res[grep("HEADER 1", res)+1], "<div class=\"page-break\"></div>")
  expect_match(res[grep("HEADER 2", res)+1], "<div class=\"page-break\"></div>")
  # For tex document this is unchanged
  res <- .generate_md_and_convert(rmd, "latex_document")
  expect_match(res[grep("HEADER 1", res)+2], "\\newpage", fixed = TRUE)
  expect_match(res[grep("HEADER 2", res)+2], "\\pagebreak", fixed = TRUE)
})
