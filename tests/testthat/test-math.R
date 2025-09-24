test_that("check_math_argument()", {
  # valid value
  expect_null(check_math_argument(NULL))
  expect_identical(check_math_argument("katex"), list(engine = "katex", url = NULL))
  expect_identical(check_math_argument(list("katex")), list(engine = "katex", url = NULL))
  expect_identical(check_math_argument(list(engine = "katex")), list(engine = "katex", url = NULL))
  expect_identical(check_math_argument(list(engine = "katex", url = "CDN")), list(engine = "katex", url = "CDN"))
  # invalid value
  expect_error(check_math_argument(list(wrong = "katex")), "'math' can be the") # wrong name
  expect_error(check_math_argument(FALSE), "'math' can be the") # logical
  expect_error(check_math_argument(TRUE), "'math' can be the")
  expect_error(check_math_argument(list(engine = FALSE)), "'math' can be the")
})

test_that("mathjax_to_math() brings backward compatibility", {
  expect_null(mathjax_to_math(NULL, "default"))
  expect_null(mathjax_to_math(FALSE, "default"))
  expect_identical(mathjax_to_math(TRUE, "katex"), "katex")
  expect_identical(mathjax_to_math("default", "webtex"), "webtex")
  expect_identical(mathjax_to_math("local", "webtex"), list(engine = "mathjax", url = "local"))
  expect_identical(mathjax_to_math("CDN", "webtex"), list(engine = "mathjax", url = "CDN"))
})

test_that("add_math_support() builds correct Pandoc arguments", {
  test_add_math_support <- function(math, template = NULL, files_dir = NULL, output_dir = NULL) {
    add_math_support(math, template, files_dir, output_dir)
  }
  expect_null(test_add_math_support(math = NULL))
  expect_identical(
    test_add_math_support(list(engine = "default")),
    test_add_math_support(list(engine = "mathjax"))
  )
  expect_error(test_add_math_support(list(engine = "dummy")), "`math_method='dummy'`")

  expect_identical(
    test_add_math_support(list(engine = "gladtex")),
    list(args = pandoc_math_args("gladtex"))
  )

  expect_identical(
    test_add_math_support(list(engine = "mathjax"), template = "default"),
    list(
      args = c(pandoc_math_args("mathjax"), pandoc_variable_arg("mathjax-url", default_mathjax())),
      extra_dependencies = NULL
    )
  )
  expect_identical(
    test_add_math_support(list(engine = "katex"), template = "default"),
    list(
      args = pandoc_math_args("katex"),
      extra_dependencies = list(html_dependency_katex())
    )
  )
})

test_that("html_dependency_katex() supports custom url", {
  expect_identical(html_dependency_katex("cdn/katex@2.1.2")$version, "2.1.2")
  expect_identical(html_dependency_katex("cdn/katex@2.1.2")$src$href, "cdn/katex@2.1.2")
  expect_identical(html_dependency_katex("cdn/katex")$version, "99.99.99")
  expect_identical(html_dependency_katex("cdn/katex")$src$href, "cdn/katex")
})

test_that("Local mathjax does not work with self_contained", {
  expect_error(validate_self_contained(list(engine = "mathjax", url = "local")),
               "isn't compatible")
})
