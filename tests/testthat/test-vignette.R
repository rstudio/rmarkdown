context("html_vignette format")

.generate_temp_vignette <- function(title = NULL,
                                    indexentry = NULL,
                                    envir = parent.frame()) {
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
  withr::defer(unlink(tmp_rmd), envir = envir)
  tmp_rmd
}

# https://github.com/rstudio/rmarkdown/pull/1789#issuecomment-616908700
test_that("vignette_pre_processor warns against differences in vignette index entry and title", {
  withr::local_options(list(rmarkdown.html_vignette.check_title = TRUE))
  input_file <- .generate_temp_vignette('document title', 'vignette title')
  # only warns for R 3.6 and later
  if (getRversion() >= 3.6) {
    expect_warning(vignette_pre_processor(input_file), 'rmarkdown.html_vignette.check_title')
  } else {
    expect_null(vignette_pre_processor(input_file))
  }
})

# https://github.com/rstudio/rmarkdown/issues/1978
test_that("vignette_pre_processor correctly handles encoding", {
  withr::local_options(list(rmarkdown.html_vignette.check_title = TRUE))
  input_file <- .generate_temp_vignette('Données', 'Données')
  expect_null(vignette_pre_processor(input_file))
})
