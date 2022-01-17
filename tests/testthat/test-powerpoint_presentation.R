test_that("Incremental feature", {
  skip_if_not_pandoc()
  if (pandoc_available("2.15")) {
    ppt_out <- powerpoint_presentation(incremental = TRUE)
    expect_true("--incremental" %in% ppt_out$pandoc$args)
  } else {
    expect_warning(ppt_out <- powerpoint_presentation(incremental = TRUE),
                   "is supported since Pandoc 2.15", fixed = TRUE)
    expect_false("--incremental" %in% ppt_out$pandoc$args)
  }
})
