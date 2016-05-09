context("notebook")

test_that("an example R Notebook document can be rendered and parsed", {

  # generate the file
  path <- test_path("resources/r-notebook.Rmd")
  file <- tempfile(fileext = ".nb.html")
  on.exit(unlink(file), add = TRUE)
  rmarkdown::render(path, output_file = file, quiet = TRUE)

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

  # set up output_source hook
  png_path <- normalizePath(testthat::test_path("resources/tinyplot.png"), winslash = "/")
  output_options <- list(output_source = function(code, context, ...) {

    label <- context$label

    if (label == "chunk-one") {
      return(summary(cars))
    }

    if (label == "chunk-two") {
      return(html_notebook_output_png(png_path))
    }

    if (label == "chunk-three") {
      library(dygraphs)
      widget <- dygraph(nhtemp, main = "New Haven Temperatures") %>%
        dyRangeSelector(dateWindow = c("1920-01-01", "1960-01-01"))
      return(widget)
    }

    if (label == "chunk-four") {

      styles <- list(
        "background-color" = "#4AF",
        "width" = "100px",
        "height" = "100px",
        "border" = "1px solid black",
        "box-shadow" = "0 0 10px #ACE"
      )

      pasted <- paste(names(styles), styles, sep = ": ", collapse = "; ")
      format <- '<div style="%s"><pre style="margin-top: 30px; text-align: center;">Box!</pre></div>'
      knitr::asis_output(sprintf(format, pasted))
    }

  })

  input_file <- testthat::test_path("resources/r-notebook.Rmd")
  # output_file <- "~/Desktop/output.nb.html"
  output_file <- tempfile(fileext = ".nb.html")
  on.exit(unlink(output_file), add = TRUE)
  render(input_file, output_options = output_options, output_file = output_file, quiet = TRUE)

  # parse notebook
  parsed <- parse_html_notebook(output_file)

})
