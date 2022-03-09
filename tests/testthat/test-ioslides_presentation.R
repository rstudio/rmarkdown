# TODO: to remove when switching the package to edition 3
local_edition(3)

test_that("Only mathjax is supported", {
  expect_error(ioslides_presentation(math_method = "katex"))
})
