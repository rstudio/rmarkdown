context("html_vignette format")

.generate_temp_vignette <- function(title = NULL,
                                    indexentry = NULL) {
  tmp_rmd <- tempfile("vignette-", fileext = ".Rmd")
  xfun::write_utf8(
    c("---",
      if (!is.null(title)) sprintf("title: %s", title),
      "output: html_vignette",
      "vignette: >",
      if(!is.null(indexentry)) {
        sprintf("    %%\\VignetteIndexEntry{%s}", indexentry)
      },
      "    %\\VignetteEngine{knitr::rmarkdown}",
      "    %\\VignetteEncoding{UTF-8}",
      "---",
      "\n# This is a vignette"),
    tmp_rmd)
  tmp_rmd
}

test_that("pre_processor adds title metadata from vignette index", {
  vignette_index <- "vignette title"
  input_file <- .generate_temp_vignette(indexentry = vignette_index)
  metadata <- yaml_front_matter(input_file)
  expect_equal(vignette_pre_processor(metadata, input_file),
               c("--metadata", sprintf("title=%s", vignette_index)))
  unlink(input_file)
})

test_that("pre_processor do nothing if rmd title exists", {
  input_file <- .generate_temp_vignette(title = "rmd title")
  metadata <- yaml_front_matter(input_file)
  expect_null(vignette_pre_processor(metadata, input_file))
  unlink(input_file)
})

test_that("pre_processor do nothing if no index entry is provided", {
  vignette_index <- "vignette title"
  input_file <- .generate_temp_vignette(NULL, NULL)
  metadata <- yaml_front_matter(input_file)
  expect_null(vignette_pre_processor(metadata, input_file))
  unlink(input_file)
  input_file <- .generate_temp_vignette("rmd title", NULL)
  metadata <- yaml_front_matter(input_file)
  expect_null(vignette_pre_processor(metadata, input_file))
  unlink(input_file)
})

# vignette with no title use the vignette index entry
# https://github.com/rstudio/rmarkdown/issues/1765
test_that("Vignette index entry is used as title if none provided", {
  vignette_index <- "vignette title"
  rmd <- .generate_temp_vignette(indexentry = vignette_index)
  res <- rmarkdown::render(rmd, quiet = TRUE)
  html <- xfun::read_utf8(res)
  title_regex <- sprintf("<title>%s</title>", vignette_index)
  expect_true(any(grepl(title_regex, html)))
  h1_regex <- sprintf("<h1[^>]*>%s</h1>", vignette_index)
  expect_true(any(grepl(h1_regex, html)))
  unlink(c(res, rmd))
})


