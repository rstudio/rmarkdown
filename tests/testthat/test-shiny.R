test_that("file.path.ci returns correctly no matter the case", {
  tmp_dir <- withr::local_tempdir()
  expect_equal(file.path.ci(tmp_dir, "global.R"), file.path(tmp_dir, "global.R"))

  withr::local_dir(tmp_dir)
  expect_equal_file <- function(file, tmp_dir, default = file) {
    withr::local_file(file); xfun::write_utf8("#dummy", file)
    expect_equal(file.path.ci(!!tmp_dir, "global.R"), file.path(!!tmp_dir, !!default))
  }
  expect_equal_file("global.R", tmp_dir)
  # on windows case in filename does not matter
  # & MacOs in GHA is case insensitive
  if (xfun::is_linux()) expect_equal_file("global.r", tmp_dir)
  expect_equal_file("global.R", "donotexist")
  expect_equal_file("global.Rmd", tmp_dir, "global.R")
})


test_that("set_current_theme() informs shiny::getCurrentTheme()", {
  expect_null(shiny::getCurrentTheme())
  theme <- bslib::bs_theme()
  set_current_theme(theme)
  expect_equal(theme, shiny::getCurrentTheme())
  set_current_theme(NULL)
  expect_null(shiny::getCurrentTheme())
})
