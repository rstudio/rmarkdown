test_that("Incremental feature", {
  skip_if_not_pandoc()
  ppt_out <- powerpoint_presentation(incremental = TRUE)
  expect_true("--incremental" %in% ppt_out$pandoc$args)
})


