context("params")

test_that("setting of params works", {

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

test_that("params_get_input", {
  # input derived from value type.
  expect_equal(params_get_input(list(
    value = TRUE
  )), "checkbox")
  expect_equal(params_get_input(list(
    value = as.integer(42)
  )), "numeric")
  expect_equal(params_get_input(list(
    value = 7.5
  )), "numeric")
  expect_equal(params_get_input(list(
    value = "character"
  )), "text")
  expect_equal(params_get_input(list(
    value = Sys.Date()
  )), "date")
  expect_equal(params_get_input(list(
    value = Sys.time()
  )), "datetime")

  # numeric becomes a slider with both min and max
  expect_equal(params_get_input(list(
    value = as.integer(42),
    min = 0
  )), "numeric")
  expect_equal(params_get_input(list(
    value = as.integer(42),
    max = 100
  )), "numeric")
  expect_equal(params_get_input(list(
    value = as.integer(42),
    min = 0,
    max = 100
  )), "slider")
  expect_equal(params_get_input(list(
    value = 7.5,
    min = 0
  )), "numeric")
  expect_equal(params_get_input(list(
    value = 7.5,
    max = 10
  )), "numeric")
  expect_equal(params_get_input(list(
    value = 7.5,
    min = 0,
    max = 10
  )), "slider")

  # choices implies radio or select
  expect_equal(params_get_input(list(
    value = "red",
    choices = c("red", "green", "blue")
  )), "radio")
  expect_equal(params_get_input(list(
    value = "red",
    multiple = TRUE,
    choices = c("red", "green", "blue")
  )), "select")
  expect_equal(params_get_input(list(
    value = "red",
    choices = c("red", "green", "blue", "yellow", "black", "white")
  )), "select")
  expect_equal(params_get_input(list(
    value = "red",
    multiple = TRUE,
    choices = c("red", "green", "blue", "yellow", "black", "white")
  )), "select")

  # explicit input overrides inference
  expect_equal(params_get_input(list(
    input = "slider",
    value = 42
  )), "slider")
  expect_equal(params_get_input(list(
    input = "file",
    value = "data.csv"
  )), "file")
})

test_that("params hidden w/o show_default", {

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

test_that("ui value conversion", {
  emptydt <- Sys.time()[-1]

  # Sys.time() -> as.character loses sub-second precision
  nows <- as.character(Sys.time())
  now <- as.POSIXct(nows)
  expect_null(params_value_from_ui(shiny::textInput, now, "not-a-datetime"),
              "produce NULL when datetime cannot be parsed")
  expect_identical(params_value_from_ui(shiny::textInput, now, nows), now,
               "produce datetime from valid UI textual value")
  expect_identical(params_value_from_ui(shiny::textInput, now, ""), emptydt,
               "produce empty POSIXct on empty input")
  expect_identical(params_value_from_ui(shiny::textInput, "not-a-datetime", "uivalue"), "uivalue",
               "textInput unmodified when datetime not involved")
  expect_identical(params_value_from_ui(shiny::numericInput, 13, 42), 42,
               "numericInput values pass through")
})
