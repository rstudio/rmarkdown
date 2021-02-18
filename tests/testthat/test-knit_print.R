test_that("knit_print.data.frame default to print without error", {
  df <- data.frame(a = 10)
  class(df) <- c("df", class(df))
  print.df <- function(x) {cat("Nb row:", nrow(x)) }
  expect_output(knit_print.data.frame(df), "Nb row:")
  expect_error(knit_print.data.frame(df, inline = TRUE), NA)
  expect_output(knit_print.data.frame(df, inline = TRUE), "Nb row:")
})
