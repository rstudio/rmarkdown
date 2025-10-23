test_that("all elements can be NULL", {
  out_fmt <- output_format(
    knitr = NULL, pandoc = NULL, keep_md = NULL, clean_supporting = NULL,
    allow_uptree_lib_dir = NULL
  )
  lapply(out_fmt, expect_null)
})

test_that("inherits base format", {
  base_fmt <- html_document()
  base_fmt$file_scope <- identity
  out_fmt <- output_format(
    knitr = NULL, pandoc = NULL, keep_md = NULL, clean_supporting = NULL,
    allow_uptree_lib_dir = NULL,
    base_format = base_fmt
  )
  classes <- lapply(base_fmt, class)
  expect_identical(lapply(out_fmt, class), classes)
  not_fun <- vapply(classes, function(x) !"function" %in% x, NA)
  expect_identical(out_fmt[not_fun], base_fmt[not_fun])
})

test_that("file_scope replaces base format", {
  base_fmt <- html_document()
  base_fmt$file_scope <- function(input_file, file_scope, ...) {
    return(list("foo" = "foo"))
  }
  out_fmt <- output_format(
    knitr = NULL, pandoc = NULL, keep_md = NULL, clean_supporting = NULL,
    file_scope = function(input_file, file_scope, ...) {
      file_scope$bar <- "bar"
      return(file_scope)
    },
    base_format = base_fmt
  )
  expect_identical(out_fmt$file_scope("input", NULL),
                   list("foo" = "foo", "bar" = "bar"))
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

test_that("output format dependency has no effect on merge if it is empty", {
  fmt <- html_document_base()
  dep <- output_format_dependency("empty")
  merged <- merge_output_format_dependencies(fmt, list(dep))
  testthat::expect_named(merged, names(fmt))
  fmt$on_exit <- merged$on_exit
  expect_identical(fmt, merged)
})

test_that("output format dependencies can be merged", {
  empty_format <- output_format(
    knitr = NULL, pandoc = NULL, keep_md = NULL, clean_supporting = NULL,
    pre_processor = NULL, post_processor = NULL,
    file_scope = NULL, on_exit = NULL
  )

  f1 <- function(...) cat("d1")
  f2 <- function(...) cat("d2")
  d1 <- output_format_dependency(
    name = "d1",
    pandoc = list(to = "html", lua_filters = "d1.lua"),
    pre_processor = f1,
    post_processor = f1,
    file_scope = f1,
    on_exit = f1
  )
  d2 <- output_format_dependency(
    name = "d2",
    pandoc = list(to = "markdown", lua_filters = "d2.lua"),
    pre_processor = f2,
    post_processor = f2,
    file_scope = f2,
    on_exit = f2
  )

  m1 <- merge_output_format_dependencies(empty_format, list(d1, d2))
  m2 <- merge_output_format_dependencies(empty_format, list(d1, d2, d2))
  expect_identical(
    m1$pandoc, list(to = "markdown", lua_filters = c("d1.lua", "d2.lua"))
  )
  expect_equal(m1, m2)

  expect_cat <- function(obj, expected) {
    expect_identical(capture.output(obj), expected)
  }
  expect_cat(invisible(m1$pre_processor()), "d1d2")
  expect_cat(m1$post_processor(output_file = "example"), "d2d1")
  expect_cat(m1$file_scope(), "d1d2")
  expect_cat(m1$on_exit(), "d1d2")
})

test_that("output format dependencies are merged once per name", {
  fmt <- html_document_base()
  d1 <- output_format_dependency("d1", pandoc = list(lua_filters = "d1.lua"))
  d2 <- output_format_dependency("d2", pandoc = list(lua_filters = "d2.lua"))
  expect_equal(
    merge_output_format_dependencies(fmt, list(d1, d2)),
    merge_output_format_dependencies(fmt, list(d1, d1, d2, d1, d2))
  )
})
