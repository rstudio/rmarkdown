context("HTML parsing")

accumulated <- data.frame(tag = c(), attribute = c(), value = c())

reset_accumulator <- function () {
  accumulated <<- data.frame(tag = c(), attribute = c(), value = c())
}

html_accumulator <- function(tag, att, val, idx) {
  accumulated <<- rbind(accumulated, data.frame(
    tag = tag, 
    attribute = att,
    value = val,
    stringsAsFactors = FALSE))
}

test_that("different attribute quoting styles are supported", {
  call_resource_attrs(paste(
    "<img src='123'/>",
    "<img src=\"456\"/>"), html_accumulator)
  expect_equal(accumulated, data.frame(
    tag = c("img", "img"),
    attribute = c("src", "src"),
    value = c("123", "456"),
    stringsAsFactors = FALSE))
  reset_accumulator()
})

test_that("irrelevant white space is ignored", {
  call_resource_attrs(paste(
    "<  img src =   \n",
    "        \t '123'\n",
    "        \t value ='abc'  />",
    "<link href=    '456'>"), html_accumulator)
  expect_equal(accumulated, data.frame(
    tag = c("img", "link"),
    attribute = c("src", "href"),
    value = c("123", "456"),
    stringsAsFactors = FALSE))
  reset_accumulator()
})

test_that("common resource types are found in a simple document", {
  call_resource_attrs(paste(
    "<!DOCTYPE html>\n",
    "<HTML>\n",
    "<HEAD>\n",
    "  <SCRIPT SRC=\"foo.js\"></SCRIPT>\n",
    "  <LINK REL=\"stylesheet\" HREF=\"bar.css\"/>\n",
    "</HEAD>\n",
    "<BODY>\n",
    "  <IMG SRC=\"baz.png\"/>\n",
    "  <IFRAME SRC=\"quux.html\"/>\n",
    "</BODY>\n",
    "</HTML>\n"), html_accumulator)
  expect_equal(accumulated, data.frame(
    tag = c("script", "link", "img", "iframe"),
    attribute = c("src", "href", "src", "src"),
    value = c("foo.js", "bar.css", "baz.png", "quux.html"),
    stringsAsFactors = FALSE))
  reset_accumulator()
})
