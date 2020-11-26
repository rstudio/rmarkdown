test_that("Correct anchor_sections style is used", {
  deps <- html_dependency_anchor_sections
  expect_error(deps(FALSE))
  expect_s3_class(deps(TRUE), "html_dependency")
  css <- function(anchor_sections, id = 2)
    deps(anchor_sections)$stylesheet[[id]]
  expect_equal(css(TRUE, 1), "anchor-sections.css")
  expect_equal(css(list(style = "icon"),1), "anchor-sections.css")
  expect_match(css(TRUE), "hash")
  expect_match(css(list(style = "icon")), "icon")
  expect_match(css(list(style = "symbol")), "symbol")
  expect_error(css(list(notexisting = "dummy")))
  expect_error(css("icon"))
})
