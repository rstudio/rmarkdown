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
  expect_equal(accumulated, data.frame(
    tag = c("h1", "h2", "h3"),
    attribute = c("align", "align", "align"),
    value = c("center", "left", "right"),
    stringsAsFactors = FALSE))
  reset_accumulator()
})

test_that("irrelevant white space is ignored", {
  html_extract_values(paste(
    "<  input type =   \n",
    "        \t 'text'\n",
    "        \t value ='abc'  />",
    "<button></button>"), html_accumulator)
  expect_equal(accumulated, data.frame(
    tag = c("input", "input"),
    attribute = c("type", "value"),
    value = c("text", "abc"),
    stringsAsFactors = FALSE))
  reset_accumulator()
})

test_that("comments are ignored", {
  html_extract_values(paste(
    "<!--img src='foo.png'-->\n",
    "<img src='bar.png'>\n",
    "<!-- <img src='baz.png'> -->\n",
    "<img src='quux.png'>\n"), html_accumulator)
  expect_equal(accumulated, data.frame(
    tag = c("img", "img"),
    attribute = c("src", "src"),
    value = c("bar.png", "quux.png"),
    stringsAsFactors = FALSE))
  reset_accumulator()
})

test_that("common resource types are found in a simple document", {
  html_extract_values(paste(
    "<!DOCTYPE html>\n",
    "<HTML>\n",
    "<HEAD>\n",
    "  <SCRIPT SRC=\"foo.js\"></SCRIPT>\n",
    "  <LINK REL=\"stylesheet\" HREF=\"bar.css\"/>\n",
    "</HEAD>\n",
    "<BODY>\n",
    "  <IMG SRC=\"baz.png\"/>\n",
    "</BODY>\n",
    "</HTML>\n"), html_accumulator)
  expect_equal(accumulated, data.frame(
    tag = c("SCRIPT", "LINK", "LINK", "IMG"),
    attribute = c("SRC", "REL", "HREF", "SRC"),
    value = c("foo.js", "stylesheet", "bar.css", "baz.png"),
    stringsAsFactors = FALSE))
  reset_accumulator()
})
