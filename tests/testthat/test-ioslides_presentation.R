test_that("Only mathjax is supported", {
  expect_error(ioslides_presentation(math_method = "katex"))
})
