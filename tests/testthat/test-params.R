context("params_html_head")

anyHeadParams <- list(
  html_head_style="style",
  html_head_style_link = "link",
  html_head_script = "script",
  html_head_script_link = "scriptjs"
)

test_that("adds default style and script", {
  result <- params_html_head()$children
  expect_equal(length(result), 2)
  expect_equal(result[[1]]$name, "style")
  expect_equal(result[[2]]$name, "script")
})

test_that("adds custom style after default style", {
  result <- do.call(params_html_head, anyHeadParams)$children
  expect_equal(length(result), 6)
  expect_equal(result[[4]], shiny::tags$style("style"))
})

test_that("adds custom style links after default but before inline custom styles", {
  result <- do.call(params_html_head, anyHeadParams)$children
  expect_equal(length(result), 6)
  expect_equal(result[[3]], shiny::tags$link(href = "link"))
  expect_equal(result[[4]], shiny::tags$style("style"))
})

test_that("adds custom script links after default but before inline custom scripts", {
  result <- do.call(params_html_head, anyHeadParams)$children
  expect_equal(length(result), 6)
  expect_equal(result[[5]], shiny::tags$script(src = "scriptjs"))
  expect_equal(result[[6]], shiny::tags$script(shiny::HTML("script")))
})

test_that("can pass string as style link", {
  result <- params_html_head(html_head_style_link="link")$children
  expect_equal(result[[3]], shiny::tags$link(href = "link"))
})

test_that("can pass vector of strings as style link", {
  result <- params_html_head(html_head_style_link=c("link"))$children
  expect_equal(result[[3]], shiny::tags$link(href = "link"))
})

test_that("can pass string as style", {
  result <- params_html_head(html_head_style="style")$children
  expect_equal(result[[3]], shiny::tags$style("style"))
})

test_that("can pass vector of strings as style", {
  result <- params_html_head(html_head_style=c("style"))$children
  expect_equal(result[[3]], shiny::tags$style("style"))
})

test_that("can pass string as script", {
  result <- params_html_head(html_head_script="script")$children
  expect_equal(result[[3]], shiny::tags$script(shiny::HTML("script")))
})

test_that("can pass vector of strings as script", {
  result <- params_html_head(html_head_script=c("script"))$children
  expect_equal(result[[3]], shiny::tags$script(shiny::HTML("script")))
})

test_that("can pass string as script link", {
  result <- params_html_head(html_head_script_link="script")$children
  expect_equal(result[[3]], shiny::tags$script(src = "script"))
})

test_that("can pass vector of strings as script link", {
  result <- params_html_head(html_head_script_link=c("script"))$children
  expect_equal(result[[3]], shiny::tags$script(src = "script"))
})

