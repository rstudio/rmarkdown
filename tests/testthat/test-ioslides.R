
context("ioslides")

.generate_markdown_for_test <- function() {

  c("# Header1\n",
    "## Header2\n",
    "### Header3\n",
    "\nTEXT HERE\n",
    "## Header2 again\n",
    "### Header3 again\n",
    "\nTEXT HERE\n"
  )


}

mock_markdown <- function(mdtext = NULL,  outputdir = NULL, ... ) {
  # create input file
  mdfile <- tempfile(pattern = "mock_XXXXX",
                     tmpdir = outputdir,
                     fileext = ".Rmd")
  cat(mdtext, file = mdfile, sep = "\n", append = FALSE)

  # output file name
  outfile <- basename(
    tempfile(pattern = "mock_XXXXX",
             tmpdir = outputdir,
             fileext = ".html"
    )
  )
  # convert
  output <- capture.output(
    render(mdfile,
           output_dir = outputdir,
           output_file = outfile,
           ioslides_presentation(...)
    )
  )

  # read in output
  html_file <- readLines(file.path(outputdir, outfile))

  # return structure for testing properties of
  invisible(structure(
    list(
      output = output,
      html_file = html_file
      ),
    class = "mocked")
    )
}

test_ioslides_presentation <- function() {

  skip_on_cran()

  outputdir <- tempfile()
  dir.create(outputdir)
  on.exit(unlink(outputdir), add = TRUE)

  # Generate mock md file
  mdtext <- .generate_markdown_for_test()
  mock2 <- mock_markdown(mdtext = mdtext, outputdir = outputdir)

  # test argument passing to pandoc
  expect_true(any(grepl("--slide-level 2", paste(mock2$output), fixed = TRUE)))

  # test status of headers in resulting file
  # Header3 should not be a slide header
  html_file <- mock2$html_file
  header_lines <- c(
    any(grepl("<h2>Header1</h2>", html_file, fixed = TRUE)),
    any(grepl("<h2>Header2</h2>", html_file, fixed = TRUE)),
    any(grepl("<h3>Header3</h3>", html_file, fixed = TRUE)),
    any(grepl("<h2>Header2 again</h2>", html_file, fixed = TRUE)),
    any(grepl("<h3>Header3 again</h3>", html_file, fixed = TRUE))
  )
  expect_true(all(header_lines))

  # test status of css slide class
  # Only header 1 have slide class with a level
  header_classes <- c(
    any(grepl("level1.*Header1", html_file))
  )
  expect_true(header_classes)
  # but not level 2 and 3
  header_classes <- c(
    any(grepl("level2.*Header2", html_file)),
    any(grepl("level3.*Header3", html_file))
  )
  expect_false(any(header_classes))

  mock3 <- mock_markdown(mdtext = mdtext, outputdir = outputdir, slide_level = 3)
  # Place the header 3 as title slide
  rout3 <- mock3$output

  # test argument passing to pandoc
  expect_true(any(grepl("--slide-level 3", paste(rout3), fixed = TRUE)))

  # test status of headers in resulting file
  # Header3 should be a slide header
  html_file <- mock3$html_file
  header_lines <- c(
    any(grepl("<h2>Header1</h2>", html_file, fixed = TRUE)),
    any(grepl("<h2>Header2</h2>", html_file, fixed = TRUE)),
    any(grepl("<h2>Header3</h2>", html_file, fixed = TRUE)),
    any(grepl("<h2>Header2 again</h2>", html_file, fixed = TRUE)),
    any(grepl("<h2>Header3 again</h2>", html_file, fixed = TRUE))
  )
  expect_true(all(header_lines))

  # test status of css slide class
  # Header 1 and header 2 have slide class with a level
  header_classes <- c(
    any(grepl("level1.*Header1", html_file)),
    any(grepl("level2.*Header2", html_file))
  )
  expect_true(all(header_classes))
  # but not level 3
  header_classes <- c(
    any(grepl("level3.*Header3", html_file))
  )
  expect_false(header_classes)

}

test_that("test_ioslides_presentation", test_ioslides_presentation())

test_ioslides_presentation_css <- function() {

  skip_on_cran()

  outputdir <- tempfile()
  dir.create(outputdir)
  on.exit(unlink(outputdir), add = TRUE)

  # Generate mock md file for data-background
  mdtext <- c("# Slide One\n",
              "## Slide Two {data-background=#CCC}\n",
              "## Slide Three {data-background=img/test.png}\n",
              "# Slide Four {data-background=#ABCDEF}\n"
              )
  mock <- mock_markdown(mdtext = mdtext, outputdir = outputdir, self_contained = FALSE)
  html <- mock$html_file

  slide_lines <-
    c(any(grepl('<slide[^>]*class="[^"]*\\bsegue\\b[^"]*".*<h2>Slide One</h2>', html, perl = TRUE))
    ## separated to be order agnostic
    , any(grepl('<slide[^>]*class="[^"]*\\bnobackground\\b[^"]*".*<h2>Slide Two</h2>', html, perl = TRUE))
    , any(grepl('<slide[^>]*class="[^"]*\\bfill\\b[^"]*".*<h2>Slide Two</h2>', html, perl = TRUE))
    , any(grepl('<slide[^>]*style="background-color: #CCC;".*<h2>Slide Two</h2>', html, perl = TRUE))

    ## separated to be order agnostic - within values of attributes also (hence [^"]*)
    , any(grepl('<slide[^>]*class="[^"]*\\bnobackground\\b[^"]*".*<h2>Slide Two</h2>', html, perl = TRUE))
    , any(grepl('<slide[^>]*class="[^"]*\\bfill\\b[^"]*".*<h2>Slide Two</h2>', html, perl = TRUE))
    , any(grepl('<slide[^>]*style="[^"]*background-image: url\\(img/test.png\\);[^"]*".*<h2>Slide Three</h2>', html))
    , any(grepl('<slide[^>]*style="[^"]*background-size: contain;[^"]*".*<h2>Slide Three</h2>', html))

    ## separated to be order agnostic
    , any(grepl('<slide[^>]*class="[^"]*\\bsegue\\b[^"]*".*<h2>Slide Four</h2>', html, perl = TRUE))
    , any(grepl('<slide[^>]*class="[^"]*\\bnobackground\\b[^"]*".*<h2>Slide Four</h2>', html, perl = TRUE))
    , any(grepl('<slide[^>]*class="[^"]*\\bfill\\b[^"]*".*<h2>Slide Four</h2>', html, perl = TRUE))
    , any(grepl('<slide[^>]*class="[^"]*\\blevel1\\b[^"]*".*<h2>Slide Four</h2>', html, perl = TRUE))
    , any(grepl('<slide[^>]*style="background-color: #ABCDEF;".*<h2>Slide Four</h2>', html, perl = TRUE))

  )
  expect_true(all(slide_lines), info = "slide lines - style attribute")

  # Generate mock md file for data-background
  plot <- file.path(getwd(), 'resources', 'tinyplot.png')
  mdtext <- c(paste0("## BG Slide {data-background=", plot, "}\n"))
  mock <- mock_markdown(mdtext = mdtext, outputdir = outputdir, self_contained = TRUE)
  html <- mock$html_file

  slide_lines <-
    c(any(grepl('<slide[^>]*style="[^"]*background-image: url\\(data:image/png;base64,[^\\)]*);[^"]*".*<h2>BG Slide</h2>', html))
      ## still separate
    , any(grepl('<slide[^>]*style="[^"]*background-size: contain;[^"]*".*<h2>BG Slide</h2>', html))
    )
  expect_true(all(slide_lines), info = "slide lines - self contained image")
}


test_that("ioslides presentation is styled", test_ioslides_presentation_css())
