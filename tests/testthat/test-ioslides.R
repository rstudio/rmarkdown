
context("ioslides")

.generate_markdown_for_test <- function (){
	
	c( "# Header1",
			"## Header2",
			"### Header3",
			"\nTEXT HERE",
			"## Header2 again\n",
			"### Header3 again",
			"\nTEXT HERE\n"
	)
	
	
}

test_ioslides_presentation <- function() {
	
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
			grep("<h2>Header1</h2>", html_file, fixed = TRUE),
			grep("<h2>Header2</h2>", html_file, fixed = TRUE),
			grep("<h3>Header3</h3>", html_file, fixed = TRUE),
			grep("<h2>Header2 again</h2>", html_file, fixed = TRUE),
			grep("<h3>Header3 again</h3>", html_file, fixed = TRUE)
	)
	expect_true(all(header_lines > 0))
	
	# test status of css slide class
	# Only header 1  have slide class with a level
	header_classes <- c(
			grep("level1.*Header1", html_file, value = TRUE)
	)
	expect_true(all(header_classes > 0))
	
	
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
	# Header3 should not be a slide header
	html_file <- readLines(file.path(outputdir, outfile))
	header_lines <- c(
			grep("<h2>Header1</h2>", html_file, fixed = TRUE),
			grep("<h2>Header2</h2>", html_file, fixed = TRUE),
			grep("<h2>Header3</h2>", html_file, fixed = TRUE),
			grep("<h2>Header2 again</h2>", html_file, fixed = TRUE),
			grep("<h2>Header3 again</h2>", html_file, fixed = TRUE)
	)
	expect_true(all(header_lines > 0))
	
	# test status of css slide class
	# Only header 1 and header 2 have slide class with a level
	header_classes <- c(
			grep("level1.*Header1", html_file, value = TRUE),
			grep("level2.*Header2", html_file, value = TRUE)
	)
	expect_true(all(header_classes > 0))
	
}

test_that("test_ioslides_presentation", test_ioslides_presentation())