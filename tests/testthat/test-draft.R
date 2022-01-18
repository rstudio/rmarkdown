test_that("available_templates() correctly returns template", {
  expect_null(available_templates("donotexist"))
  expect_identical(available_templates(), dir(pkg_file("rmarkdown/templates")))
  expect_identical(available_templates(full_path = TRUE),
                   dir(pkg_file("rmarkdown/templates"), full.names = TRUE))
})

