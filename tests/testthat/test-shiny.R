test_that("file.path.ci returns correctly no matter the case", {
  # TODO: added for new tests - to remove when switching the package to edition 3
  local_edition(3)
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


test_that("set_current_theme() informs shiny::getCurrentTheme() only with bslib theme", {
  skip_if_not(packageVersion("shiny") >= 1.6)
  with_clean_shinyTheme <- function(expr) {
    shiny:::setCurrentTheme(NULL)
    force(expr)
    shiny:::setCurrentTheme(NULL)
  }
  with_clean_shinyTheme({
    theme <- bslib::bs_theme()
    set_current_theme(theme)
    expect_equal(theme, shiny::getCurrentTheme())
  })
  with_clean_shinyTheme({
    set_current_theme(NULL)
    expect_null(shiny::getCurrentTheme())
  })
  with_clean_shinyTheme({
    set_current_theme("cerulean")
    expect_null(shiny::getCurrentTheme())
  })
})

test_that("html_prerendered is a full document template to use as UI for shiny", {
  tmp_rmd <- local_rmd_file(c("---", "title: shiny", "runtime: shiny_prerendered", "---", "", "```{r}", "1+1", "```"))
  html <- shiny_prerendered_html(tmp_rmd, list(quiet = TRUE))
  expect_match(html, "<!-- HEAD_CONTENT -->")
})

test_that("html can be annotated as being a full document with deps attached", {
  html <- HTML("dummy")
  deps <- list(htmltools::htmlDependency("a", "1.1", c(href = "/")))
  ui <- shiny_prerendered_ui(html, deps)
  expect_s3_class(ui, "html_document")
  expect_equal(htmltools::htmlDependencies(ui), deps)
})
