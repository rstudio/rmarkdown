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
