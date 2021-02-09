local_rmd_file <- function(..., .env = parent.frame()) {
  path <- withr::local_tempfile(envir = .env, fileext = ".Rmd")
  xfun::write_utf8(c(...), path)
  path
}
