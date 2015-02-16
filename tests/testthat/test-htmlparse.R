context("HTML parsing")

accumulated <- data.frame(tag = c(), attribute = c(), value = c())

reset_accumulator <- function () {
  accumulated <<- data.frame(tag = c(), attribute = c(), value = c())
}

html_accumulator <- function(tag, att, val) {
  accumulated <<- rbind(accumulated, data.frame(
    tag = tag, 
    attribute = att,
    value = val,
    stringsAsFactors = FALSE))
}

test_that("different attribute quoting styles are supported", {
  html_extract_values(paste(
    "<h1 align='center'></h1>",
    "<h2 align=\"left\"></h2>",
    "<h3 align=right></h3>"), html_accumulator)
  browser()
  expect_equal(accumulated, data.frame(
    tag = c("h1", "h2", "h3"),
    attribute = c("align", "align", "align"),
    value = c("center", "left", "right"),
    stringsAsFactors = FALSE))
})
