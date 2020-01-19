context("yaml front matter")

test_that("yaml header is correctly parsed", {
  tmp_file <- tempfile(fileext = ".Rmd")
  on.exit(unlink(tmp_file), add = TRUE)
  yaml_header <- c(
    "---",
    "title: test",
    "output: html_document",
    "---"
  )
  xfun::write_utf8(yaml_header,
                   tmp_file)
  yml_parsed <- list(title = "test", output = "html_document")
  expect_equal(yaml_front_matter(tmp_file),
               yml_parsed)
  xfun::write_utf8(c("", "", yaml_header),
                   tmp_file)
  expect_equal(yaml_front_matter(tmp_file),
               yml_parsed)
  xfun::write_utf8(c("", "<!-- rnb-text-begin -->", "", yaml_header),
                   tmp_file)
  expect_equal(yaml_front_matter(tmp_file),
               yml_parsed)
  xfun::write_utf8(c("", "anything", "", yaml_header),
                   tmp_file)
  expect_length(yaml_front_matter(tmp_file), 0L)
})
