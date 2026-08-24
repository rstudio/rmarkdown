test_that("ioslides renders figures with Pandoc 3", {
  skip_if_not(rmarkdown::pandoc_available("3.0"))

  input <- tempfile(fileext = ".md")
  output <- tempfile(fileext = ".html")
  source_writer <- system.file(
    "rmd/ioslides/ioslides_presentation.lua",
    package = "rmarkdown"
  )
  writer <- tempfile(fileext = ".lua")
  writeLines(c(
    "local fig_caption = true",
    "local incremental = false",
    "local smaller = false",
    "local smart = true",
    "local slide_level = 2",
    readLines(source_writer)
  ), writer)
  writeLines(c("# Slide", "", "![caption](image.jpg)"), input)

  messages <- system2(
    rmarkdown::pandoc_exec(),
    c("--from=markdown", "--to", writer, "--output", output, input),
    stdout = TRUE,
    stderr = TRUE
  )

  expect_false(any(grepl("Undefined function 'Figure'", messages, fixed = TRUE)))
  rendered <- paste(readLines(output), collapse = "\n")
  expect_match(rendered, "image.jpg", fixed = TRUE)
  expect_match(rendered, "caption", fixed = TRUE)
})
