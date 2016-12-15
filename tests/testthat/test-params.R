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

test_that("params render their UI", {

  skip_on_cran()

  # file input is always NULL
  ui <- params_value_to_ui(shiny::fileInput, "anything", TRUE)
  expect_null(ui)

  # text input is a pass-through
  ui <- params_value_to_ui(shiny::textInput, "some char", TRUE)
  expect_equal(ui, "some char")

  # Date gets converted to char
  dat <- Sys.time()
  ui <- params_value_to_ui(shiny::textInput, dat, TRUE)
  expect_equal(ui, as.character(dat))

  # NULLs get massaged for numeric/slider
  ui <- params_value_to_ui(shiny::numericInput, NULL, TRUE)
  expect_equal(ui, 0)
  ui <- params_value_to_ui(shiny::sliderInput, NULL, TRUE)
  expect_equal(ui, 0)

  # Everything else is a passthrough
  myobj <- list(a=123, b=NULL, c="huh")
  ui <- params_value_to_ui(NULL, myobj, TRUE)
  expect_equal(ui, myobj)
})
