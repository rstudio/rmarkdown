# TODO: added for new tests - to remove when switching the package to edition 3
local_edition(3)

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
