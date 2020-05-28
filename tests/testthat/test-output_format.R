test_that("all elements can be NULL", {
  out_fmt <- output_format(
    knitr = NULL, pandoc = NULL, keep_md = NULL, clean_supporting = NULL
  )
  lapply(out_fmt, expect_null)
})

test_that("inherits base format", {
  base_fmt <- html_document()
  out_fmt <- output_format(
    knitr = NULL, pandoc = NULL, keep_md = NULL, clean_supporting = NULL,
    base_format = base_fmt
  )
  classes <- lapply(base_fmt, class)
  expect_identical(lapply(out_fmt, class), classes)
  not_fun <- vapply(classes, function(x) !"function" %in% x, NA)
  expect_identical(out_fmt[not_fun], base_fmt[not_fun])
})

test_that("clean_supporting is coerced to FALSE only if keep_md is TRUE", {
  args_combinations <- expand.grid(
    knitr = list(NULL),
    pandoc = list(NULL),
    keep_md = list(TRUE, FALSE, NULL),
    clean_supporting = list(TRUE, FALSE, NULL)
  )

  results <- do.call(
    function(...) Map(function(...) output_format(...)$clean_supporting, ...),
    args_combinations
  )

  expected <- ifelse(
    vapply(args_combinations$keep_md, isTRUE, NA),
    FALSE,
    args_combinations$clean_supporting
  )

  expect_identical(results, expected)
})
