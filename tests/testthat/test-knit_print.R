test_that("knit_print.data.frame default to print without error", {
  df <- data.frame(a = 10)
  expect_error(knit_print.data.frame(df, inline = TRUE), NA)
})
