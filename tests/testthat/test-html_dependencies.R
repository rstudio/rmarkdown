test_that("Correct anchor_sections style is used", {
  deps <- html_dependency_anchor_sections
  expect_s3_class(deps(), "html_dependency")
  expect_error(deps("dummy"), "should be one of")
  expect_match(deps()$stylesheet[[2]], "anchor-sections-hash.css")
  expect_null(deps()$script[[1]])
  expect_equal(deps(section_divs = TRUE)$script[[1]], "anchor-sections.js")
})
