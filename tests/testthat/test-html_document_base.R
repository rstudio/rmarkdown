test_that("default email obfuscation parameter is used", {
  skip_if_not(pandoc_available("1.17.2"))
  expect_false("--email-obfuscation" %in% html_document_base()$pandoc$args)
})
