
context("ioslides")

.generate_markdown_for_test <- function (){

  c("# Header1\n",
    "## Header2\n",
    "### Header3\n",
    "\nTEXT HERE\n",
    "## Header2 again\n",
    "### Header3 again\n",
    "\nTEXT HERE\n"
  )


}

test_ioslides_presentation <- function() {

  skip_on_cran()

  outputdir <-  tempfile()
  dir.create(outputdir)
  on.exit(unlink(outputdir), add = TRUE)

  # Generate mock md file
  mdtext <- .generate_markdown_for_test()
  mdfile <- file.path(outputdir, "mock.Rmd")
  cat(mdtext, file = mdfile, sep = "\n")

  # test conversion
  outfile <- "mock_default.html"
  rout2 <- capture.output(
    render(mdfile,
           output_dir = outputdir,
           output_file = outfile,
           ioslides_presentation()
    )
  )

  # test argument passing to pandoc
  expect_true(any(grepl("--slide-level 2", paste(rout2), fixed = TRUE)))

  # test status of headers in resulting file
  # Header3 should not be a slide header
  html_file <- readLines(file.path(outputdir, outfile))
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


  # Place the header 3 as title slide
  rout3 <- capture.output(
    render(mdfile,
           output_dir = outputdir,
           output_file = outfile,
           ioslides_presentation(slide_level = 3)
    )
  )

  # test argument passing to pandoc
  expect_true(any(grepl("--slide-level 3", paste(rout3), fixed = TRUE)))

  # test status of headers in resulting file
  # Header3 should be a slide header
  html_file <- readLines(file.path(outputdir, outfile))
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
