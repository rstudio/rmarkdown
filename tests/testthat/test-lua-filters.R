.generate_md_and_convert <- function(content, output_format) {
  input_file <- local_rmd_file(c("---\ntitle: Test\n---\n", content))
  .render_and_read(input_file, output_format = output_format)
}

# rmarkdown requires pandoc >= 2.1 to support Lua filters
skip_if_not_pandoc("2.1")

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
  rmd <- c(paste0(headers, " ", numbers, "\n\n"), "[1]")
  expected <- paste(numbers, numbers)
  # pandoc 2.11.2 default to atx headers
  if (pandoc_available("2.11.2")) expected <- paste(headers, expected)

  # -gfm_auto_identifiers
  result <- .generate_md_and_convert(rmd, md_document(number_sections = TRUE))
  expect_identical(result[result %in% expected], expected)
  expect_false(identical(result[length(result)], "[1](#1-1)"))

  # +gfm_auto_identifiers
  result <- .generate_md_and_convert(
    rmd,
    md_document(number_sections = TRUE, md_extensions = "+gfm_auto_identifiers")
  )
  expect_identical(result[result %in% expected], expected)
  expect_identical(result[length(result)], "[1](#1-1)")
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

test_that("lua file are correctly found", {
  expect_match(basename(pkg_file_lua()),  ".*[.]lua$")
  expect_match(basename(pkg_file_lua("number-sections.lua")),  "^number-sections.lua$")
})
