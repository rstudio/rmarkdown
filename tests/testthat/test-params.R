context("params")

test_that("setting of params works", {

  skip_on_cran()

  params_sample <- '---\ntitle: "test"\noutput: html_document\nparams:\n  field1:\n    value: "defaulthere"\n  field2: null\n  field3: \n  field4: NULL\n---'

  # No overrides
  params <- knit_params_get(params_sample, NULL)
  expect_equal(params$field1, "defaulthere")
  expect_null(params$field2)
  expect_null(params$field3)
  expect_null(params$field4)

  # With overrides
  params <- knit_params_get(params_sample, list(field1 = "new value"))
  expect_equal(params$field1, "new value")

  # With overrides for NULL/null/empty params
  params <- knit_params_get(params_sample, list(field2 = NULL,field4 = "value"))
  expect_null(params$field2)
  expect_equal(params$field4, "value")

  # Invalid
  expect_error(knit_params_get(params_sample, "new value"))

  # Params not declared in YAML
  expect_error(knit_params_get(params_sample, list(field5 = "a",field6 = "b",field7 = NULL)), regexp = "field5, field6, field7$")
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
  myobj <- list(a = 123, b = NULL, c = "huh")
  ui <- params_value_to_ui(NULL, myobj, TRUE)
  expect_equal(ui, myobj)
})

test_that("parameters are configurable", {
  # Unknown input types are not configurable.
  expect_error(params_configurable(list(
      input = "unsupported")))

  # Numeric (and most other controls) do not support multiple values.
  expect_true(params_configurable(list(
      input = "numeric",
      value = 42)))
  expect_false(params_configurable(list(
      input = "numeric",
      value = c(13, 42))))

  # Selectors permit multiple values if explicitly enabled.
  expect_true(params_configurable(list(
      input = "select",
      choices = c(1, 2, 3, 4),
      value = 2)))
  expect_true(params_configurable(list(
      input = "select",
      multiple = TRUE,
      choices = c(1, 2, 3, 4),
      value = c(2, 4))))
  expect_false(params_configurable(list(
      input = "select",
      multiple = FALSE,
      choices = c(1, 2, 3, 4),
      value = c(2, 4))))
  expect_false(params_configurable(list(
      input = "select",
      choices = c(1, 2, 3, 4),
      value = c(2, 4))))

  # Sliders permit either one or two values (singular or double-ended slider).
  expect_true(params_configurable(list(
      input = "slider",
      min = 1,
      max = 100,
      value = 50)))
  expect_true(params_configurable(list(
      input = "slider",
      min = 1,
      max = 100,
      value = c(45, 55))))
  expect_false(params_configurable(list(
      input = "slider",
      min = 1,
      max = 100,
      value = c(10, 20, 30))))
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
  myobj <- list(a = 123, b = NULL, c = "huh")
  ui <- params_value_to_ui(function(){}, myobj, FALSE)
  expect_equal(ui, NULL)
})
