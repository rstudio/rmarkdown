# TODO: to remove when switching the package to edition 3
local_edition(3)

test_that("check_math_argument()", {
  expect_null(check_math_argument(NULL))
  expect_identical(check_math_argument("katex"), list(engine = "katex", url = NULL))
  expect_identical(check_math_argument(list("katex")), list(engine = "katex", url = NULL))
  expect_identical(check_math_argument(list(engine = "katex", url = "CDN")), list(engine = "katex", url = "CDN"))
  expect_error(check_math_argument(list(wrong = "katex")), "math can be the")
})

test_that("add_math_support() builds correct Pandoc arguments", {
  test_add_math_support <- function(math, template = NULL, files_dir = NULL, output_dir = NULL) {
    add_math_support(math, template, files_dir, output_dir)
  }
  expect_null(test_add_math_support(math = NULL))
  expect_identical(test_add_math_support("default"), test_add_math_support("mathjax"))
  expect_error(test_add_math_support(list(engine = "dummy")), "Engine `math='dummy'`")

  expect_identical(test_add_math_support(list(engine = "gladtex")), pandoc_math_args("gladtex"))

  expect_identical(
    test_add_math_support(list(engine = "mathjax"), template = "default"),
    c(pandoc_math_args("mathjax"), pandoc_variable_arg("mathjax-url", default_math("mathjax")))
  )
  expect_identical(
    test_add_math_support(list(engine = "katex"), template = "default"),
    c(pandoc_math_args("katex"), pandoc_variable_arg("katex-url", default_math("katex")))
  )

  withr::with_tempdir({
    dir.create(file_dir <- "files-dir")
    expect_identical(
      test_add_math_support(list(engine = "mathjax", url = "local"), files_dir = file_dir, output_dir = "."),
      c("--mathjax", file.path("files-dir", "mathjax-local", mathjax_config()))
    )
  })
})
