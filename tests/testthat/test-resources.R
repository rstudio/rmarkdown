context("resource discovery")

sort_resources <- function(resources) {
  # sort by filename and remove rownames to avoid errors arising from file ordering
  # -- we don't really care what order these come back in
  resources <- as.data.frame(resources[order(resources[[1]]), , drop = FALSE])
  rownames(resources) <- NULL
}

test_that("R Markdown resource discovery finds expected resources", {
  # Test with the current version of the template
  file.copy(pkg_file("rmd/h/default.html"), test_path('resources/template.html'), overwrite = TRUE)
  resources <- find_external_resources(test_path("resources/rmarkdown.Rmd"))
  expected <- data.frame(
    path = c("empty.md", "empty.png", "empty.tsv", "empty.Rmd", "empty.css",
             "empty.jpg", "empty.html", "template.html", "empty.csv"),
    explicit = c(FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    web      = c(FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE,  TRUE,  FALSE,  FALSE),
    stringsAsFactors = FALSE)

  resources <- sort_resources(resources)
  expected <- sort_resources(expected)

  expect_equal(resources, expected)
})

test_that("R Markdown resource discovery finds expected scss when sass is used", {
  skip_if_not_installed("sass")
  css_files <- c("empty.scss", "empty.css", "empty2.scss", "empty2.css")
  yaml <- yaml::as.yaml(list(
    title = "test",
    output = list(
      html_document = list(
        css = c("empty.scss", "empty.css", "empty2.scss", "empty2.css"),
        theme = "cerulean"
      )
    )
  ))
  rmd <- local_rmd_file(knitr::knit_expand(text = c("---", "{{yaml}}", "---", "", "# test")))
  withr::local_dir(dirname(rmd))
  file.create(css_files)
  resources <- find_external_resources(basename(rmd))
  resource <- resources[resources$path %in% css_files, ]
  expected <- data.frame(
    path = css_files,
    explicit = rep_len(FALSE, length(css_files)),
    web      = !needs_sass(css_files),
    stringsAsFactors = FALSE)
  resource <- sort_resources(resource)
  expected <- sort_resources(expected)
  expect_equal(resource, expected)
})

test_that("R Markdown resource discovery finds expected scss and css when bslib is used", {
  skip_if_not_installed("bslib")
  css_files <- c("empty.scss", "empty.css", "empty2.scss", "empty2.css")
  yaml <- yaml::as.yaml(list(
    title = "test",
    output = list(
      html_document = list(
        css = c("empty.scss", "empty.css", "empty2.scss", "empty2.css"),
        theme = list(version = "5")
      )
    )
  ))
  rmd <- local_rmd_file(knitr::knit_expand(text = c("---", "{{yaml}}", "---", "", "# test")))
  withr::local_dir(dirname(rmd))
  file.create(css_files)
  resources <- find_external_resources(basename(rmd))
  resource <- resources[resources$path %in% css_files, ]
  expected <- data.frame(
    path = css_files,
    explicit = rep_len(FALSE, length(css_files)),
    web      = rep_len(FALSE, length(css_files)),
    stringsAsFactors = FALSE)
  resource <- sort_resources(resource)
  expected <- sort_resources(expected)
  expect_equal(resource, expected)
})


test_that("HTML resource discovery finds expected resources", {

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
  resources <- find_external_resources("resources/period.Rmd")
  expect_equal(nrow(resources), 0)
})


test_that("dependencies in .R files are recursively discovered", {
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
  # save current working directory
  oldwd <- setwd(test_path("resources"))
  on.exit(setwd(oldwd), add = TRUE)

  file.rename("file-exists.Rmd", "file exists.Rmd")
  capture.output(unlink(render("file exists.Rmd")))
  file.rename("file exists.Rmd", "file-exists.Rmd")
  expect_true(file.exists("empty.csv"))
})

test_that("resources not deleted when intermediates_dir is same as input", {
  # save current working directory
  oldwd <- setwd(test_path("resources"))
  on.exit(setwd(oldwd), add = TRUE)
  capture.output(unlink(render("file-exists.Rmd", intermediates_dir = ".")))
  expect_true(file.exists("empty.csv"))
})

test_that("copy_file_with_dir does not copy or delete files outside dest dir", {
  # regression test for https://github.com/rstudio/rmarkdown/issues/2619
  # copy_file_with_dir should skip (and return NULL for) any path that resolves
  # outside the destination directory to prevent accidentally overwriting or
  # later deleting external files via unlink(intermediates)
  root <- withr::local_tempdir("copy-file-with-dir-test")
  dest_dir <- file.path(root, "dest")
  sibling_dir <- file.path(root, "sibling")
  dir.create(dest_dir)
  dir.create(sibling_dir)

  # create a file in sibling dir that should never be touched
  external_file <- file.path(sibling_dir, "external.txt")
  xfun::write_utf8("external content", external_file)

  # a path using '..' traverses outside dest_dir – should be skipped
  result <- copy_file_with_dir("../sibling/external.txt", dest_dir, sibling_dir)

  expect_null(result)                      # nothing returned for cleanup
  expect_true(file.exists(external_file))  # original file untouched
  expect_length(list.files(dest_dir), 0L)  # nothing placed in dest_dir

  # positive case: a path that stays inside dest_dir is copied normally
  xfun::write_utf8("inner content", file.path(sibling_dir, "inner.txt"))
  result2 <- copy_file_with_dir("inner.txt", dest_dir, sibling_dir)
  expect_true(file.exists(file.path(dest_dir, "inner.txt")))
  expect_equal(result2, normalizePath(file.path(dest_dir, "inner.txt")))
})

test_that("external files not deleted when render() uses intermediates_dir", {
  # regression test for https://github.com/rstudio/rmarkdown/issues/2619:
  # render() with intermediates_dir must not delete external input files that
  # are referenced from the Rmd but live outside intermediates_dir
  skip_if_not_pandoc()

  root <- withr::local_tempdir("intermediates-dir-test")
  input_dir <- file.path(root, "input")
  data_dir  <- file.path(root, "data")
  out_dir   <- file.path(root, "output")
  dir.create(input_dir)
  dir.create(data_dir)
  dir.create(out_dir)

  # external data file in sibling directory – must survive render()
  external_file <- file.path(data_dir, "external.csv")
  xfun::write_utf8("x,y\n1,2", external_file)

  # Rmd that references the external file via a '..' path so that it is
  # discovered by find_external_resources() and fed to copy_file_with_dir()
  rmd <- file.path(input_dir, "test.Rmd")
  xfun::write_utf8(c(
    "---", "title: Test", "output: html_document", "---", "",
    "```{r}", 'read.csv("../data/external.csv")', "```"
  ), rmd)

  output_file <- withr::local_tempfile(fileext = ".html")
  suppressMessages(render(rmd, output_file = output_file,
                          intermediates_dir = out_dir, quiet = TRUE))

  expect_true(file.exists(output_file),
    info = "Render must complete successfully and produce output")
  expect_true(file.exists(external_file),
    info = "External file must not be deleted by render() with intermediates_dir")
})

test_that("empty quoted strings don't confuse resource discovery", {
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

test_that("multiple resources in the includes option can be discovered", {
  resources <- find_external_resources("resources/multi-includes.Rmd")
  expected <- data.frame(
    path = c("dep1.html", "dep2.html"),
    explicit = FALSE,
    web      = FALSE,
    stringsAsFactors = FALSE)

  expect_equal(resources, expected)
})

test_that("knitr child are correctly discovered as resources from chunk options", {
  expect_child_resource_found <- function(child_opts, child) {
    dir <- withr::local_tempdir("find-child")
    withr::local_dir(dir)
    rmd <- "test.Rmd"
    xfun::write_utf8(
      text = knitr::knit_expand(text = c("```{r,<<child_opts>>}", "```"), delim = c("<<", ">>")),
      rmd
    )
    xfun::write_utf8(c("Content"), child)
    expect_contains(find_external_resources(!!rmd)$path, !!child)
  }
  child <- "child.Rmd"
  expect_child_resource_found('child="child.Rmd"', child)
  expect_child_resource_found(' child = "child.Rmd"', child)
  expect_child_resource_found('child=c("child.Rmd")', child)
  expect_child_resource_found("child='child.Rmd'", child)
  expect_child_resource_found(" child = 'child.Rmd'", child)
  expect_child_resource_found("child=c('child.Rmd')", child)
})
