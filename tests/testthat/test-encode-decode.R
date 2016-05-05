context("encoding")

test_that("R objects can be encoded, decoded", {
  data <- list(a = 1L, b = 2L, c = "3")
  encoded <- base64_encode_object(data)
  decoded <- base64_decode_object(encoded)
  expect_equal(data, decoded)
})
