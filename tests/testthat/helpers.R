local_rmd_file <- function(..., .env = parent.frame()) {
  path <- withr::local_tempfile(.local_envir = .env, fileext = ".Rmd")
  xfun::write_utf8(c(...), path)
  path
}

.render_and_read <- function(input, ...) {
  skip_if_not_pandoc()
  output_file <- withr::local_tempfile()
  res <- rmarkdown::render(input, output_file = output_file, quiet = TRUE, ...)
  xfun::read_utf8(res)
}

# Use to test pandoc availability or version lower than
skip_if_not_pandoc <- function(ver = NULL) {
  if (!pandoc_available(ver)) {
    msg <- if (is.null(ver)) {
      "Pandoc is not available"
    } else {
      sprintf("Version of Pandoc is lower than %s.", ver)
    }
    skip(msg)
  }
}

# Use to test version greater than
skip_if_pandoc <- function(ver = NULL) {
  if (pandoc_available(ver)) {
    msg <- if (is.null(ver)) {
      "Pandoc is available"
    } else {
      sprintf("Version of Pandoc is greater than %s.", ver)
    }
    skip(msg)
  }
}

