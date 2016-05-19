context("resource discovery")

test_that("R Markdown resource discovery finds expected resources", {

  skip_on_cran()

  resources <- find_external_resources("resources/rmarkdown.Rmd")
  expected <- data.frame(
    path = c("empty.md", "empty.png", "empty.tsv", "empty.Rmd", "empty.css",
             "empty.jpg", "empty.html", "empty.csv"),
    explicit = c(FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE),
    web      = c(FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE,  TRUE,  FALSE),
    stringsAsFactors = FALSE)

  # sort by filename to avoid errors arising from file ordering -- we don't
  # really care what order these come back in
  resources <- as.data.frame(resources[order(resources[[1]]), , drop = FALSE])
  expected <- as.data.frame(expected[order(expected[[1]]), , drop = FALSE])

  expect_equal(resources, expected)
})


test_that("HTML resource discovery finds expected resources", {

  skip_on_cran()

  resources <- find_external_resources("resources/html.html")
  expected <- data.frame(
    path = c("empty.js", "empty.css", "empty.png"),
    explicit = c(FALSE, FALSE, FALSE),
    web      = c(TRUE,  TRUE,  TRUE),
    stringsAsFactors = FALSE)

  resources <- as.data.frame(resources[order(resources[[1]]), , drop = FALSE])
  expected <- as.data.frame(expected[order(expected[[1]]), , drop = FALSE])
  expect_equal(resources, expected)
})

test_that("Vanilla Markdown resource discovery finds expected resources", {

  skip_on_cran()

  resources <- find_external_resources("resources/markdown.md")
  expected <- data.frame(
    path = c("empty.png", "empty.jpg"),
    explicit = c(FALSE, FALSE),
    web      = c(TRUE,  TRUE ),
    stringsAsFactors = FALSE)

  resources <- as.data.frame(resources[order(resources[[1]]), , drop = FALSE])
  expected <- as.data.frame(expected[order(expected[[1]]), , drop = FALSE])
  expect_equal(resources, expected)
})


test_that("PDF-specific resources are discovered", {

  skip_on_cran()

  resources <- find_external_resources("resources/pdf.Rmd")
  expected <- data.frame(
    path = c("empty.bib", "empty.csl", "empty.png"),
    explicit = c(FALSE, FALSE, FALSE),
    web      = c(FALSE, FALSE, TRUE ),
    stringsAsFactors = FALSE)

  resources <- as.data.frame(resources[order(resources[[1]]), , drop = FALSE])
  expected <- as.data.frame(expected[order(expected[[1]]), , drop = FALSE])
  expect_equal(resources, expected)
})


test_that("bare relative directory references are ignored", {
  skip_on_cran()

  resources <- find_external_resources("resources/period.Rmd")
  expect_equal(nrow(resources), 0)
})


test_that("dependencies in .R files are recursively discovered", {
  skip_on_cran()

  resources <- find_external_resources("resources/readcsv.Rmd")
  expected <- data.frame(
    path = c("empty.csv", "readcsv.R", "readcsv-source.R"),
    explicit = c(FALSE, FALSE, FALSE),
    web      = c(FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE)

  resources <- as.data.frame(resources[order(resources[[1]]), , drop = FALSE])
  expected <- as.data.frame(expected[order(expected[[1]]), , drop = FALSE])
  expect_equal(resources, expected)
})

test_that("implicitly discovered directories are ignored", {
  skip_on_cran()

  resources <- find_external_resources("resources/directory-refs.Rmd")
  expected <- data.frame(
    path = c("nonempty/empty.csv", "nonempty/empty.jpg"),
    explicit = c(TRUE, TRUE),
    web      = c(FALSE, TRUE),
    stringsAsFactors = FALSE)

  resources <- as.data.frame(resources[order(resources[[1]]), , drop = FALSE])
  expected <- as.data.frame(expected[order(expected[[1]]), , drop = FALSE])
  expect_equal(resources, expected)
})

test_that("resource_files use cases all work", {
  skip_on_cran()

  resources <- find_external_resources("resources/resource-files.Rmd")
  expected <- data.frame(
    path = c("nonempty/empty.csv", "csvs/csv1.csv", "csvs/csv2.csv", "csvs/other/csv3.csv", "empty.bib"),
    explicit = c(TRUE, TRUE, TRUE, TRUE, TRUE),
    web      = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE)

  resources <- as.data.frame(resources[order(resources[[1]]), , drop = FALSE])
  expected <- as.data.frame(expected[order(expected[[1]]), , drop = FALSE])
  expect_equal(resources, expected)
})

test_that("dependencies can be discovered on .R files directly", {
  skip_on_cran()

  resources <- find_external_resources("resources/readcsv-source.R")
  expected <- data.frame(
    path = c("empty.csv", "readcsv.R"),
    explicit = c(FALSE, FALSE),
    web      = c(FALSE, FALSE),
    stringsAsFactors = FALSE)

  resources <- as.data.frame(resources[order(resources[[1]]), , drop = FALSE])
  expected <- as.data.frame(expected[order(expected[[1]]), , drop = FALSE])
  expect_equal(resources, expected)
})

test_that("filenames with shell characters can use relative resource paths", {
  skip_on_cran()

  # save current working directory
  oldwd <- getwd()
  on.exit(setwd(oldwd), add = TRUE)

  file.rename("resources/file-exists.Rmd", "resources/file exists.Rmd")
  on.exit(file.rename("resources/file exists.Rmd", "resources/file-exists.Rmd"), add = TRUE)

  # render the file (contains an expression that stops if its resource is not
  # present)
  capture.output(output_file <- render("resources/file exists.Rmd"))
  on.exit(unlink(output_file), add = TRUE)
})

test_that("resources not deleted when filenames contain shell characters", {
  skip_on_cran()

  # save current working directory
  oldwd <- setwd("resources")
  on.exit(setwd(oldwd), add = TRUE)

  file.rename("file-exists.Rmd", "file exists.Rmd")
  capture.output(unlink(render("file exists.Rmd")))
  file.rename("file exists.Rmd", "file-exists.Rmd")
  expect_true(file.exists("empty.csv"))
})

test_that("empty quoted strings don't confuse resource discovery", {
  skip_on_cran()

  resources <- find_external_resources("resources/quotes.Rmd")
  expected <- data.frame(
    path = c("empty.csv", "empty.tsv"),
    explicit = c(FALSE, FALSE),
    web      = c(FALSE, FALSE),
    stringsAsFactors = FALSE)

  resources <- as.data.frame(resources[order(resources[[1]]), , drop = FALSE])
  expected <- as.data.frame(expected[order(expected[[1]]), , drop = FALSE])
  expect_equal(resources, expected)
})

test_that("resources are discovered in CSS files", {
  skip_on_cran()

  resources <- find_external_resources("resources/has-css.Rmd")
  expected <- data.frame(
    path = c("empty.png", "has-image.css"),
    explicit = c(FALSE, FALSE),
    web      = c(TRUE, TRUE),
    stringsAsFactors = FALSE)

  resources <- as.data.frame(resources[order(resources[[1]]), , drop = FALSE])
  expected <- as.data.frame(expected[order(expected[[1]]), , drop = FALSE])
  expect_equal(resources, expected)
})

test_that("resources are discovered in notebook files", {
  skip_on_cran()

  resources <- find_external_resources("resources/r-notebook.Rmd")
  expected <- data.frame(
    path = c("tinyplot.png"),
    explicit = c(FALSE),
    web      = c(TRUE),
    stringsAsFactors = FALSE)

  resources <- as.data.frame(resources[order(resources[[1]]), , drop = FALSE])
  expected <- as.data.frame(expected[order(expected[[1]]), , drop = FALSE])
  expect_equal(resources, expected)
})
