local_rmd_file <- function(..., .env = parent.frame()) {
  path <- withr::local_tempfile(envir = .env, fileext = ".Rmd")
  xfun::write_utf8(c(...), path)
  path
}

skip_if_not_pandoc <- function(ver) {
  if (!pandoc_available(ver)) {
    skip(sprintf("Version of Pandoc is lower than %s.", ver))
  }
}

skip_if_pandoc <- function(ver) {
  if (pandoc_available(ver)) {
    skip(sprintf("Version of Pandoc is greater than %s.", ver))
  }
}
