context("html_vignette format")

# vignette with no title use the vignette index entry
# https://github.com/rstudio/rmarkdown/issues/1765
test_that("Vignette index entry is used as title if none provided", {
  res <- rmarkdown::render('resources/vignette-no-title.Rmd')
  html <- xfun::read_utf8(res)
  expect_true(any(grepl("<title>vignette title</title>", html)))
  expect_true(any(grepl("<h1[^>]*>vignette title</h1>", html)))
  unlink(res)
})

test_that("html_vignette only use index entry if no title provided", {
  input_file <- 'resources/vignette-no-title.Rmd'
  metadata <- yaml_front_matter(input_file)
  expect_equal(vignette_pre_processor(metadata, input_file),
               c("--metadata", "title=vignette title"))
  metadata <- c(title = "rmd title", metadata)
  expect_null(vignette_pre_processor(metadata, input_file))
})



