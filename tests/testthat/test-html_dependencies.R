# TODO: to remove when switching the package to edition 3
local_edition(3)

test_that("Dependencies are correctly validated", {
  # not a html dep
  expect_error(validate_html_dependency(list(a = 1)), "is not of class html_dependency", fixed = TRUE)
  skip_if_not_installed("htmltools")
  # file based dep
  dep <- htmlDependency(name = "foo", version = "1.1.0", src = pkg_file("rmd/h"), script = "foo.js")
  expect_identical(validate_html_dependency(dep), dep)
  # href base dep
  dep <- htmlDependency(name = "foo", version = "1.1.0", src = c(href = "https://example.org"), script = "foo.js")
  expect_identical(validate_html_dependency(dep), dep)
  # incomplete html deps
  dep2 <- dep; dep2$name <- NULL
  expect_error(validate_html_dependency(dep2), "name .* not provided")
  dep2 <- dep; dep2$version <- NULL
  expect_error(validate_html_dependency(dep2), "version .* not provided")
  dep2 <- dep; dep2$src <- NULL
  expect_error(validate_html_dependency(dep2), "neither path nor href")
  dep2 <- dep; dep2$src <- list(file = tempfile("donotexist"))
  expect_error(validate_html_dependency(dep2), "path for html_dependency not found:", fixed = TRUE)
})



