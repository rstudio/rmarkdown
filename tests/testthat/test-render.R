# TODO: to remove when switching the package to edition 3
local_edition(3)

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

test_that("file_scope split correctly input file", {
  skip_if_not(getRversion() >= '3.5')
  rmd <- local_rmd_file(c("# H1", "content1", "# H2", "content2"))
  file_scope_fun <- function(file) {
    x <- xfun::read_utf8(file)
    list(
      list(name = "A", content = x[1:2]),
      list(name = "B", content = x[3:4])
    )
  }
  splitted <- file_scope_split(rmd, file_scope_fun)
  expect_true(all(file.exists(splitted)))
  on.exit(unlink(splitted), add = TRUE, after = FALSE)
  expect_match(splitted, "[.]split[.]md$")
  expect_snapshot_file(splitted[1])
  expect_snapshot_file(splitted[2])
})
