context("html_vignette format")

# vignette with no title use the vignette index entry
# https://github.com/rstudio/rmarkdown/issues/1765
test_that("If no title, vignette index entry is used", {
  res <- rmarkdown::render('resources/vignette-no-title.Rmd')
  html <- xfun::read_utf8(res)
  expect_true(any(grepl("<title>vignette title</title>", html)))
  expect_true(any(grepl("<h1[^>]*>vignette title</h1>", html)))
  unlink(res)
})

test_that("If a title, the title is used", {
  rmd <- xfun::read_utf8('resources/vignette-no-title.Rmd')
  tmp_rmd <- tempfile("test-vignette-", fileext = ".Rmd")
  xfun::write_utf8(c(rmd[1], "title: rmd title", rmd[-1]), tmp_rmd)
  res <- rmarkdown::render(tmp_rmd)
  html <- xfun::read_utf8(res)
  expect_true(any(grepl("<title>rmd title</title>", html)))
  expect_true(any(grepl("<h1[^>]*>rmd title</h1>", html)))
  unlink(c(res, tmp_rmd))
})

