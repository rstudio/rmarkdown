.generate_md_and_convert <- function(content, output_format) {
  input_file <- local_rmd_file(c("---\ntitle: Test\n---\n", content))
  res <- .render_and_read(input_file, output_format = output_format)
  # print nicely for snapshot test
  xfun::raw_string(res)
}

# rmarkdown requires pandoc >= 2.1 to support Lua filters
skip_if_not_pandoc("2.1")

# TODO: At some point, change this file to test only the filter in a simple pandoc conversion without all the rendering ( ~ unit test for Lua filters)

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
  headers <- c("# A", "## B", "# C", "## D")
  rmd <- c(paste0(headers, "\n\n"), "See [A]")
  # Variant for snapshot: pandoc 2.11.2 default to atx headers
  pandoc_versions <- if (pandoc_available("3")) {
    "pandoc-3"
  } else if (pandoc_available("2.18.0.1")) {
    "after-pandoc-2.18"
  } else if (pandoc_available("2.18")) {
    "pandoc-2.18"
  } else if (pandoc_available("2.11.2")) {
    "after-pandoc-2.11.2"
  } else {
    "before-pandoc-2.11.2"
  }
  # -gfm_auto_identifiers
  result <- .generate_md_and_convert(rmd, md_document(number_sections = TRUE))
  expect_snapshot_output(result, variant = pandoc_versions)

  # +gfm_auto_identifiers
  skip_if_not_pandoc("2.5") # gfm_auto_identifiers is not working the same before
  result <- .generate_md_and_convert(
    rmd,
    md_document(number_sections = TRUE, md_extensions = "+gfm_auto_identifiers")
  )
  expect_snapshot_output(result, variant = pandoc_versions)

  # Github document
  skip_if_not_pandoc("2.10.1") # changes in gfm writer break this test for earlier versions
  result <- .generate_md_and_convert(rmd, github_document(number_sections = TRUE, toc = TRUE))
  expect_snapshot_output(result, variant = pandoc_versions)
})

test_that("latex-divs.lua works with HTML doc", {
  rmd <- "::: custom\ncontent\n:::\n\n some other content"
  res <- .generate_md_and_convert(rmd, "html_document")
  expect_match(grep('custom', res, value = TRUE), '<div class="custom"')
  rmd <- "::: {.custom #id}\ncontent\n:::\n\n some other content"
  res <- .generate_md_and_convert(rmd, "html_document")
  expect_match(grep('custom', res, value = TRUE), '<div id="id" class="custom">')
  rmd <- "::: {.custom #id data-latex=''}\ncontent\n:::\n\n some other content"
  res <- .generate_md_and_convert(rmd, "html_document")
  expect_match(grep('custom', res, value = TRUE), '<div id="id" class="custom">')
  rmd <- "::: {.custom #id latex=true}\ncontent\n:::\n\n some other content"
  res <- .generate_md_and_convert(rmd, "html_document")
  expect_match(grep('custom', res, value = TRUE), '<div id="id" class="custom">')
  rmd <- "::: {.custom #id latex=1}\ncontent\n:::\n\n some other content"
  res <- .generate_md_and_convert(rmd, "html_document")
  expect_match(grep('custom', res, value = TRUE), '<div id="id" class="custom">')
})

test_that("latex-divs.lua works with LaTeX (PDF)", {
  rmd <- "::: {.custom #id}\ncontent\n:::\n\n some other content"
  res <- .generate_md_and_convert(rmd, "latex_document")
  expect_false(any(grepl('custom', res)))
  rmd <- "::: {.custom #id data-latex=''}\ncontent\n:::\n\n some other content"
  res <- .generate_md_and_convert(rmd, "latex_document")
  lines <- local({ i <- grep("custom", res); seq.int(i[1], i[2])})
  expect_snapshot_output(cat(res[lines]))
  rmd <- "::: {.custom #id latex=true}\ncontent\n:::\n\n some other content"
  res <- .generate_md_and_convert(rmd, "latex_document")
  lines <- local({ i <- grep("custom", res); seq.int(i[1], i[2])})
  expect_snapshot_output(cat(res[lines]))
  rmd <- "::: {.custom #id latex=1}\ncontent\n:::\n\n some other content"
  res <- .generate_md_and_convert(rmd, "latex_document")
  lines <- local({ i <- grep("custom", res); seq.int(i[1], i[2])})
  expect_snapshot_output(cat(res[lines]))
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
  # different lua filter
  pgb <- "pagebreak"; lxd <- "latex-div"
  tbl <- if (pandoc_available("3.2.1")) "table-classes"
  nbs <- "number-sections"; acs <- "anchor-sections"
  expect_filters(beamer_presentation(), c(pgb, lxd))
  expect_filters(github_document(number_sections = TRUE),
                 md_document(number_sections = TRUE))
  expect_filters(html_document(), c(pgb, lxd, tbl))
  expect_filters(html_document(anchor_sections = TRUE), c(pgb, lxd, tbl, acs))
  expect_filters(html_document(anchor_sections = list(depth = 3)), c(pgb, lxd, tbl, acs))
  expect_filters(html_document_base(), c(pgb, lxd, tbl))
  expect_filters(latex_document(), c(pgb, lxd))
  expect_filters(context_document(ext = ".tex"), c(pgb))
  expect_filters(md_document(number_sections = TRUE), c(nbs))
  expect_filters(pdf_document(), c(pgb, lxd))
  expect_filters(powerpoint_presentation(number_sections = TRUE), c(nbs))
  expect_filters(odt_document(number_sections = TRUE), c(pgb, nbs))
  expect_filters(rtf_document(number_sections = TRUE), c(nbs))
  expect_filters(slidy_presentation(number_sections = TRUE), c(pgb, lxd, tbl, nbs))
  expect_filters(
    word_document(number_sections = TRUE),
    c(pgb, if (!pandoc_available("2.10.1")) nbs)
  )
})

test_that("lua file are correctly found", {
  expect_match(basename(pkg_file_lua()),  ".*[.]lua$")
  expect_match(basename(pkg_file_lua("number-sections.lua")),  "^number-sections.lua$")
})
