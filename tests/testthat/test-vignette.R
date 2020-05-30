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

# https://github.com/rstudio/rmarkdown/pull/1789#issuecomment-616908700
test_that("vignette_pre_processor warns against differences in vignette index entry and title", {
  opts <- options(rmarkdown.html_vignette.check_title = TRUE)
  on.exit(options(opts), add = TRUE)
  input_file <- .generate_temp_vignette('document title', 'vignette title')
  expect_warning(vignette_pre_processor(input_file), 'rmarkdown.html_vignette.check_title')
  unlink(input_file)
})
