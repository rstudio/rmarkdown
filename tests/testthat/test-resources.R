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
