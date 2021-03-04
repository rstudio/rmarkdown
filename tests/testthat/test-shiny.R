# TODO: added for new tests - to remove when switching the package to edition 3
local_edition(3)

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

test_that("HTML template contains special comment when in shiny prerendered", {
  skip_if_not_pandoc()
  special_comment <- "<!-- HEAD_CONTENT -->"
  content <- c("---", "title: shiny", "runtime: shiny_prerendered", "---", "", "```{r}", "1+1", "```")
  tmp_rmd <- local_rmd_file(content)
  html <- .render_and_read(tmp_rmd, output_format = "html_document")
  expect_match(one_string(html), special_comment, fixed = TRUE,
               label = "hmlt_document template")
  html <- .render_and_read(tmp_rmd, output_format = "ioslides_presentation")
  expect_match(one_string(html), special_comment, fixed = TRUE,
               label = "ioslides_presentation template")
  html <- .render_and_read(tmp_rmd, output_format = "slidy_presentation")
  expect_match(one_string(html), special_comment, fixed = TRUE,
               label = "slidy_presentation template")
  # no runtime shiny prerendered
  content <- content[-which(grepl("^runtime", content))]
  tmp_rmd <- local_rmd_file(content)
  html <- .render_and_read(tmp_rmd, output_format = "html_document")
  expect_false(any(grepl(special_comment, html)))
})

test_that("Special HEAD comment is added if none in rendered HTML when in shiny prerendered", {
  skip_if_not_pandoc()
  special_comment <- "<!-- HEAD_CONTENT -->"
  tmp_rmd <- local_rmd_file(c("---", "title: shiny", "runtime: shiny_prerendered", "---", "", "```{r}", "1+1", "```"))
  html <- shiny_prerendered_html(tmp_rmd, list(quiet = TRUE))
  expect_length(which(special_comment == xfun::split_lines(html)), 1L)
  tmp_rmd <- local_rmd_file(c("---", "title: shiny", "runtime: shiny_prerendered", "---", "", "```{r}", "1+1", "```"))
  opts <- list(template = NULL, mathjax = NULL)
  html <- shiny_prerendered_html(tmp_rmd, list(output_options = opts, quiet = TRUE))
  expect_length(which(special_comment == xfun::split_lines(html)), 1L)
})

test_that("html can be annotated as being a full document with deps attached", {
  html <- HTML("dummy")
  deps <- list(htmltools::htmlDependency("a", "1.1", c(href = "/")))
  ui <- shiny_prerendered_ui(html, deps)
  expect_s3_class(ui, "html_document")
  expect_equal(htmltools::htmlDependencies(ui), deps)
})

# As we don't use directly `{{ headContent() }}`, this test should help detect
# if htmltools has change this special token in the future. In our CI tests, but also
# in reverse dependency test
test_that("htmtools still use the special token rmarkdown uses in its template", {
  htmltools_headcontent <- as.character(htmltools::htmlTemplate(text_ = "{{ headContent() }}"))
  expect_match(htmltools_headcontent, "<!-- HEAD_CONTENT -->", fixed = TRUE)
})
