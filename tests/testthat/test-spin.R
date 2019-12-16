context("spin")

test_that("default metadata are included if no metadata is provided", {
  content <- c(
    "#' # Header 1",
    "print('Hello World!')"
  )
  input_file <- tempfile(fileext = ".R")
  on.exit(unlink(input_file), add = TRUE)
  xfun::write_utf8(content, input_file)
  output_file <- tempfile(fileext = ".html")
  on.exit(unlink(output_file), add = TRUE)
  res <- render(input_file,
                output_format = "html_document",
                output_file = output_file,
                quiet = TRUE)
  output <- xfun::read_utf8(res)
  expect_match(grep("<title>", output, value = TRUE),
               paste0("<title>", basename(input_file), "</title>"))
})

test_that("included YAML metadata are respected", {
  content <- c(
    "#' ---",
    "#' title: Test title",
    "#' ---",
    "print('Hello World!')"
  )
  input_file <- tempfile(fileext = ".R")
  on.exit(unlink(input_file), add = TRUE)
  xfun::write_utf8(content, input_file)
  output_file <- tempfile(fileext = ".html")
  on.exit(unlink(output_file), add = TRUE)
  res <- render(input_file,
                output_format = "html_document",
                output_file = output_file,
                quiet = TRUE)
  output <- xfun::read_utf8(res)
  expect_match(grep("<title>", output, value = TRUE), "<title>Test title</title>")
})
