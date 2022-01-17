test_that("Powerpoint require Pandoc 2.0.5 and above", {
  skip_if_not_pandoc()
  skip_if_pandoc("2.0.5")
  expect_error(powerpoint_presentation(), "is required and was not found")
})

skip_if_not_pandoc("2.0.5")

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


