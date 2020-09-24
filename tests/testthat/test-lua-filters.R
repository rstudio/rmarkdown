.generate_md_and_convert <- function(content, output_format) {
  input_file <- tempfile(fileext = ".Rmd")
  output_file <- tempfile()
  on.exit(unlink(c(input_file, output_file)), add = TRUE)
  xfun::write_utf8(c("---\ntitle: Test\n---\n", content), input_file)
  res <- rmarkdown::render(input_file, output_format = output_format, output_file = output_file, quiet = TRUE)
  xfun::read_utf8(res)
}

# Lua filters exists only since pandoc 2.0
skip_if_not(rmarkdown::pandoc_available("2.0"))

test_that("pagebreak Lua filters works", {
  rmd <- "# HEADER 1\n\\newpage\n# HEADER 2\n\\pagebreak\n# HEADER 3"
  res <- .generate_md_and_convert(rmd, "html_document")
  expect_match(res[grep("HEADER 1", res)+1], "<div style=\"page-break-after: always;\"></div>")
  expect_match(res[grep("HEADER 2", res)+1], "<div style=\"page-break-after: always;\"></div>")
  # add a class instead of inline style
  rmd2 <- paste0("---\nnewpage_html_class: page-break\n---\n", rmd)
  res <- .generate_md_and_convert(rmd2, "html_document")
  expect_match(res[grep("HEADER 1", res)+1], "<div class=\"page-break\"></div>")
  expect_match(res[grep("HEADER 2", res)+1], "<div class=\"page-break\"></div>")
  # For tex document this is unchanged
  res <- .generate_md_and_convert(rmd, "latex_document")
  expect_match(res[grep("HEADER 1", res)+2], "\\newpage", fixed = TRUE)
  expect_match(res[grep("HEADER 2", res)+2], "\\pagebreak", fixed = TRUE)
})

test_that("number_sections Lua filter works", {
  numbers <- c("1", "1.1", "2", "2.1")
  headers <- c("#", "##", "#", "##")
  rmd <- paste0(headers, " ", numbers, "\n\n")
  result <- .generate_md_and_convert(rmd, md_document(number_sections = TRUE))
  expected <- paste(numbers, numbers)
  expect_identical(result[result %in% expected], expected)
})

test_that("formats have the expected Lua filter", {
  expect_filters <- function(format_fun, expected_filters) {
    filters <- basename(format_fun$pandoc$lua_filters)
    if ("rmarkdown_output_format" %in% class(expected_filters))
      expected_filters <- basename(expected_filters$pandoc$lua_filters)
    expect_identical(
      filters,
      xfun::with_ext(expected_filters, "lua")
    )
  }
  expect_filters(beamer_presentation(), c("pagebreak", "latex-div"))
  expect_filters(github_document(number_sections = TRUE),
                 md_document(number_sections = TRUE))
  expect_filters(html_document(), c("pagebreak", "latex-div"))
  expect_filters(html_document_base(), c("pagebreak", "latex-div"))
  expect_filters(latex_document(), c("pagebreak", "latex-div"))
  expect_filters(context_document(ext = ".tex"), c("pagebreak"))
  expect_filters(md_document(number_sections = TRUE), "number-sections")
  expect_filters(pdf_document(), c("pagebreak", "latex-div"))
  expect_filters(powerpoint_presentation(number_sections = TRUE),
                 "number-sections")
  expect_filters(odt_document(number_sections = TRUE),
                 c("pagebreak", "number-sections"))
  expect_filters(rtf_document(number_sections = TRUE), c("number-sections"))
  expect_filters(
    slidy_presentation(number_sections = TRUE),
    c("pagebreak", "latex-div", "number-sections")
  )
  expect_filters(
    word_document(number_sections = TRUE),
    c("pagebreak", if (!pandoc_available("2.10.1")) "number-sections"))
})
