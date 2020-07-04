test_that("Metadata is available before pre_knit", {
  message_pre_knit = 'pre_knit handles metadata'
  fmt <- md_document()
  fmt$pre_knit <- function(input, ...) {
    # `metadata` requires `rmarkdown::`-prefix. Otherwise, it becomes `list()`.
    if (identical(rmarkdown::metadata, list(foo = 'bar'))) {
      message(message_pre_knit)
    }
  }
  input_file = tempfile(fileext = '.md')
  writeLines('---\nfoo: bar\n---', input_file)
  expect_message(render(input_file, fmt, quiet = TRUE), message_pre_knit)
})
