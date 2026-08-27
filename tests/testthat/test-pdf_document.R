test_that("pdf_document() incorporates latex dependencies", {
  expected_dependencies <- c(
    "\\usepackage{longtable}", # from extra_dependencies
    "\\usepackage{hyperref}" # from knit_meta
  )

  extra_dependencies <- list(
    # do not include non-latex dependencies
    # as pre processor does not care the kind of extra dependencies
    latex_dependency("longtable")
  )
  knit_meta <- list(
    latex_dependency("hyperref"),
    html_dependency_jquery() # pre_processor should remove html dependencies
  )

  fmt <- pdf_document(extra_dependencies = extra_dependencies)

  pandoc_args <- fmt$pre_processor(
    list(),
    tempfile(),
    "static",
    knit_meta,
    tempdir(),
    tempdir()
  )

  included <- pandoc_args[which(pandoc_args == "--include-in-header") + 1L]
  expect_length(included, 1L)
  expect_true(file.exists(included))
  expect_identical(readLines(included), expected_dependencies)
})

test_that("LaTeX aux files are written next to the output, not the input dir (#1975, #1615)", {
  skip_if_not_latex()

  # separate, writable input and output directories
  input_dir <- withr::local_tempdir()
  output_dir <- withr::local_tempdir()
  input <- file.path(input_dir, "demo.Rmd")
  xfun::write_utf8(c("---", "output: pdf_document", "---", "", "Hello."), input)

  # keep aux files so we can observe where they land
  withr::local_options(tinytex.clean = FALSE)
  out <- render(input, output_dir = output_dir, quiet = TRUE)
  on.exit(unlink(out), add = TRUE)

  expect_true(file.exists(out))
  # the aux/log files must be created in the output dir ...
  expect_true(file.exists(file.path(output_dir, "demo.log")))
  # ... and NOT in the input dir, which may be read-only in production
  expect_false(file.exists(file.path(input_dir, "demo.log")))
  expect_false(file.exists(file.path(input_dir, "demo.aux")))
})
