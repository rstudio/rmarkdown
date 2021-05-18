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

test_that("Keep only args and Lua filter while merging pandoc options", {
  # non default opt
  foo_opt <- pandoc_options(
    to = "foo", from = "init",  args = "--foo",
    ext = "foo", keep_tex = TRUE, latex_engine = "xelatex",
    lua_filters = "foo.lua")
  bar_opt <- pandoc_options(to = "bar")
  res_opt <- merge_pandoc_options(foo_opt, bar_opt)
  # all options are merged except these
  kept <- c("args", "lua_filters")
  expect_equal(res_opt[!names(res_opt) %in% kept],
               bar_opt[!names(bar_opt) %in% kept])
  expect_equal(res_opt[kept], foo_opt[kept])
  # Instead they are appended
  bar_opt$args <- "--another"
  bar_opt$lua_filters <- "bar.lua"
  res_opt <- merge_pandoc_options(foo_opt, bar_opt)
  lapply(kept, function(x) expect_equal(res_opt[[x]], c(foo_opt[[x]], bar_opt[[x]])))
})

test_that("citeproc is enable/disable correctly", {
  test_citeproc_not_required <- function(yml, lines = NULL) {
    expect_false(citeproc_required(!!yml, !!lines))
  }
  test_citeproc_required <- function(yml, lines = NULL) {
    expect_true(citeproc_required(!!yml, !!lines))
  }
  test_citeproc_not_required(list(title = "dummy"))
  test_citeproc_not_required(list(title = "dummy", citeproc = TRUE))
  test_citeproc_required(list(bibliography = "a.bib"))
  test_citeproc_not_required(list(bibliography = "a.bib", citeproc = FALSE))
  test_citeproc_required(list(references = "a.bib"))
  test_citeproc_not_required(list(references = "a.bib", citeproc = FALSE))
  test_citeproc_required(list(references = list(type = "book")))
  test_citeproc_not_required(list(references = list(type = "book"), citeproc = FALSE))
  test_citeproc_required(list(), c("references:", "  - type: book"))
  test_citeproc_not_required(list(citeproc = FALSE), c("references:", "  - type: book"))
  test_citeproc_required(list(), c("bibliography:", "  - a.bib"))
  test_citeproc_not_required(list(citeproc = FALSE), c("bibliography:", "  - a.bib"))
  test_citeproc_not_required(list(title = "dummy"), c("bibliography: a.bib"))
})

test_that("default output_format is guessed from output file extension", {
  expect_equal(output_format_string_from_ext("test.pdf"), "pdf_document")
  expect_equal(output_format_string_from_ext("test.html"), "html_document")
  expect_equal(output_format_string_from_ext("test.docx"), "word_document")
  expect_equal(output_format_string_from_ext("test.any"), "html_document")
  expect_equal(output_format_string_from_ext(NULL), "html_document")
})
