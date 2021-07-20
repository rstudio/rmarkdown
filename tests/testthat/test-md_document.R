local_edition(3)

test_that("adapt_md_variant() adds extensions to markdown variants", {
  expect_identical(adapt_md_variant("markdown", TRUE), "markdown+yaml_metadata_block")
  expect_identical(adapt_md_variant("markdown_phpextra", TRUE), "markdown_phpextra+yaml_metadata_block")
  expect_identical(adapt_md_variant("markdown_mmd", TRUE), "markdown_mmd+yaml_metadata_block")
  expect_identical(adapt_md_variant("markdown_strict", TRUE), "markdown_strict+yaml_metadata_block")
  expect_identical(adapt_md_variant("markdown_github", TRUE), "markdown_github+yaml_metadata_block")

  expect_identical(adapt_md_variant("markdown", FALSE), "markdown-yaml_metadata_block")
  expect_identical(adapt_md_variant("markdown_phpextra", FALSE), "markdown_phpextra-yaml_metadata_block")
  expect_identical(adapt_md_variant("markdown_mmd", FALSE), "markdown_mmd-yaml_metadata_block")
  expect_identical(adapt_md_variant("markdown_strict", FALSE), "markdown_strict-yaml_metadata_block")
  expect_identical(adapt_md_variant("markdown_github", FALSE), "markdown_github-yaml_metadata_block")

  expect_identical(adapt_md_variant("markdown+yaml_metadata_block", TRUE), "markdown+yaml_metadata_block")
  expect_identical(adapt_md_variant("markdown-yaml_metadata_block", TRUE), "markdown-yaml_metadata_block+yaml_metadata_block")
  expect_identical(adapt_md_variant("markdown+yaml_metadata_block", FALSE), "markdown+yaml_metadata_block-yaml_metadata_block")
  expect_identical(adapt_md_variant("markdown-yaml_metadata_block", FALSE), "markdown-yaml_metadata_block")

  # ignore correctly unsupported variants
  expect_identical(adapt_md_variant("markdown_new", TRUE), "markdown_new")
  expect_identical(adapt_md_variant("markdown_new", FALSE), "markdown_new")
})

test_that("adapt_md_variant() ignored unknown variants", {
  expect_identical(adapt_md_variant("markdown_new", TRUE), "markdown_new")
  expect_identical(adapt_md_variant("markdown_new", FALSE), "markdown_new")
})

test_that("adapt_md_variant() with special variants (pandoc >= 2.13)", {
  skip_if_not_pandoc('2.13')
  expect_identical(adapt_md_variant("commonmark", TRUE), "commonmark+yaml_metadata_block")
  expect_identical(adapt_md_variant("gfm", TRUE), "gfm+yaml_metadata_block")
  expect_identical(adapt_md_variant("commonmark_x", TRUE), "commonmark_x+yaml_metadata_block")
  expect_identical(adapt_md_variant("commonmark", FALSE), "commonmark-yaml_metadata_block")
  expect_identical(adapt_md_variant("gfm", FALSE), "gfm-yaml_metadata_block")
  expect_identical(adapt_md_variant("commonmark_x", FALSE), "commonmark_x-yaml_metadata_block")

  expect_identical(adapt_md_variant("gfm+yaml_metadata_block", TRUE), "gfm+yaml_metadata_block")
  expect_identical(adapt_md_variant("gfm-yaml_metadata_block", TRUE), "gfm-yaml_metadata_block+yaml_metadata_block")
  expect_identical(adapt_md_variant("gfm+yaml_metadata_block", FALSE), "gfm+yaml_metadata_block-yaml_metadata_block")
  expect_identical(adapt_md_variant("gfm-yaml_metadata_block", FALSE), "gfm-yaml_metadata_block")

})

test_that("adapt_md_variant() with special variants (pandoc < 2.13)", {
  skip_if_pandoc('2.13')
  expect_identical(adapt_md_variant("commonmark", TRUE), "commonmark")
  expect_identical(adapt_md_variant("gfm", TRUE), "gfm")
  expect_identical(adapt_md_variant("commonmark_x", TRUE), "commonmark_x")
  expect_identical(adapt_md_variant("commonmark", FALSE), "commonmark")
  expect_identical(adapt_md_variant("gfm", FALSE), "gfm")
  expect_identical(adapt_md_variant("commonmark_x", FALSE), "commonmark_x")
})

test_that("md_document() can preserve yaml", {
  skip_on_cran() # avoid pandoc issue on CRAN
  expect_snapshot_md <- function(variant, preserve_yaml) {
    rmd <- local_rmd_file(c("---", "title: test", "---", "", "content"))
    res <- render(rmd, md_document(variant, preserve_yaml = preserve_yaml), quiet = TRUE)
    expect_snapshot_file(res, sprintf("yaml-block-%s%s.md", variant, if (preserve_yaml) "-meta" else ""))
  }
  expect_snapshot_md("markdown", preserve_yaml = FALSE)
  expect_snapshot_md("markdown_phpextra", preserve_yaml = FALSE)
  expect_snapshot_md("markdown_mmd", preserve_yaml = FALSE)
  expect_snapshot_md("markdown_strict", preserve_yaml = FALSE)
  expect_snapshot_md("markdown_github", preserve_yaml = FALSE)
  expect_snapshot_md("markdown", preserve_yaml = TRUE)
  expect_snapshot_md("markdown_phpextra", preserve_yaml = TRUE)
  expect_snapshot_md("markdown_mmd", preserve_yaml = TRUE)
  expect_snapshot_md("markdown_strict", preserve_yaml = TRUE)
  expect_snapshot_md("markdown_github", preserve_yaml = TRUE)
})

