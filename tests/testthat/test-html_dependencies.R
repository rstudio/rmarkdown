test_that("Correct anchor_sections style is used", {
  deps <- html_dependency_anchor_sections
  expect_s3_class(deps(), "html_dependency")
  expect_error(deps("dummy"))
  expect_match(deps()$stylesheet[[2]], "hash")
  expect_null(deps()$script[[1]])
  expect_equal(deps(section_divs = TRUE)$script[[1]], "anchor-sections.js")
  css <- function(style, id = 2) {
    deps(style)$stylesheet[[id]]
  }
  expect_equal(css("hash", 1), "anchor-sections.css")
  expect_equal(css("icon",1), "anchor-sections.css")
  expect_match(css("icon"), "icon")
  expect_match(css("symbol"), "symbol")
})
