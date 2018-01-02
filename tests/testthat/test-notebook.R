context("notebook")

# expect that the default evaluate hook is restored after the notebook is
# rendered and parsed
hook_evaluate_get <- function() knitr::knit_hooks$get("evaluate")
hook_evaluate <- hook_evaluate_get()
expect_default_evaluate_hook <- function() {
  expect_identical(hook_evaluate_get(), hook_evaluate)
}

test_that("an example R Notebook document can be rendered and parsed", {
  skip_on_cran()

  # check initial status of hook
  expect_default_evaluate_hook()

  # generate the file
  path <- test_path("resources/r-notebook.Rmd")
  file <- tempfile(fileext = ".nb.html")
  on.exit(unlink(file), add = TRUE)
  rmarkdown::render(path, output_file = file, quiet = TRUE)

  # confirm the evaluate hook has been restored post-render
  expect_default_evaluate_hook()

  # if running interactively, try running the following code to open the
  # generated document -- in RStudio, you should see the source .Rmd opened,
  # with outputs populated in the editor view
  #
  #     file.edit(file)

  # try parsing and validating the parse format
  parsed <- parse_html_notebook(file)
  expect_identical(parsed$rmd, read_lines_utf8(path, encoding = "UTF-8"))
})

test_that("a custom output_source can be used on render", {
  skip_on_cran()

  # set up output_source hook
  png_path <- normalizePath(testthat::test_path("resources/tinyplot.png"), winslash = "/")
  output_options <- list(output_source = function(code, context, ...) {

    label <- context$label

    if (label == "chunk-one") {
      expect_true(context$chunk.index == 1)
      output <- summary(cars)
      return(list(
        html_notebook_output_code("summary(cars)"),
        output
      ))
    }

    if (label == "chunk-two") {
      expect_true(context$chunk.index == 2)
      return(html_notebook_output_img(png_path))
    }

    if (label == "chunk-three") {
      expect_true(context$chunk.index == 3)
      if (requireNamespace("dygraphs", quietly = TRUE)) {

        library(dygraphs)
        widget <- dygraph(nhtemp, main = "New Haven Temperatures") %>%
          dyRangeSelector(dateWindow = c("1920-01-01", "1960-01-01"))

        return(list(
          html_notebook_output_code("# This is a fake comment!"),
          widget
        ))
      }

      return(html_notebook_output_html("<!-- dygraphs not installed -->"))
    }

    if (label == "chunk-four") {
      expect_true(context$chunk.index == 4)
      styles <- list(
        "background-color" = "#4AF",
        "width" = "100px",
        "height" = "100px",
        "border" = "1px solid black",
        "box-shadow" = "0 0 10px #ACE"
      )

      pasted <- paste(names(styles), styles, sep = ": ", collapse = "; ")
      format <- '<div style="%s"><pre style="margin-top: 30px; text-align: center;">Box!</pre></div>'
      rendered <- sprintf(format, pasted)
      return(html_notebook_output_html(rendered))
    }

  })

  input_file <- testthat::test_path("resources/r-notebook.Rmd")
  # output_file <- "~/Desktop/output.nb.html"
  output_file <- tempfile(fileext = ".nb.html")
  on.exit(unlink(output_file), add = TRUE)

  # check initial status of hook
  expect_default_evaluate_hook()

  render(input_file, output_options = output_options, output_file = output_file, quiet = TRUE)

  # confirm the evaluate hook has been restored post-render
  expect_default_evaluate_hook()

  # parse notebook
  parsed <- parse_html_notebook(output_file)

})
