context("params")

test_that("setting of params works", {

  skip_on_cran()

  params_sample <- '---\ntitle: "test"\noutput: html_document\nparams:\n  field1:\n    value: "defaulthere"\n---'

  # No overrides
  params <- knit_params_get(params_sample, NULL)
  expect_equal(params$field1, "defaulthere")

  # With overrides
  params <- knit_params_get(params_sample, list(field1="new value"))
  expect_equal(params$field1, "new value")

  # Invalid
  expect_error(knit_params_get(params_sample, "new value"))
})
