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

test_that("params hidden w/o show_default", {

  skip_on_cran()

  # file input is always NULL
  ui <- params_value_to_ui(shiny::fileInput, "anything", FALSE)
  expect_null(ui)

  # text input suppressed
  ui <- params_value_to_ui(shiny::textInput, "some char", FALSE)
  expect_equal(ui, NULL)

  # Date gets scrubbed
  ui <- params_value_to_ui(shiny::textInput, dat, FALSE)
  expect_equal(ui, NULL)

  # Numeric -> 0
  ui <- params_value_to_ui(shiny::numericInput, 13, FALSE)
  expect_equal(ui, 0)

  # Slider -> 0
  # TODO: should this be MIN?
  ui <- params_value_to_ui(shiny::sliderInput, 13, FALSE)
  expect_equal(ui, 0)

  # Everything else is scrubbed
  myobj <- list(a=123, b=NULL, c="huh")
  ui <- params_value_to_ui(function(){}, myobj, FALSE)
  expect_equal(ui, NULL)
})
