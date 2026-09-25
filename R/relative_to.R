
#' Relative path utility function
#'
#' Given a directory and a file, return a relative path from the directory to
#' the file, or the unmodified file path if the file does not appear to be in
#' the directory.
#'
#' @param dir Directory
#' @param file File
#'
#' @return Relative path from the directory to the file (or the unmodified file
#'   path if the file does not appear to be in the directory).
#'
#' @export
relative_to <- function(dir, file) {
  # ensure directory ends with a /
  dir <- sub('/+$', '/', paste0(dir, '/'))

  # if the file is prefixed with the directory, return a relative path
  if (identical(substr(file, 1, nchar(dir)), dir))
    file <- substr(file, nchar(dir) + 1, nchar(file))

  # simplify ./
  sub('^[.]/', '', file)
}

# A variant of relative_to that normalizes its inputs.
normalized_relative_to <- function(dir, file) {
  relative_to(
    normalize_path(dir, must_work = FALSE),
    normalize_path(file, must_work = FALSE))
}

# Use xfun's syntactic check: same_path(path, file.path('.', path)) does not
# work here because on Unix, normalizePath(mustWork = FALSE) leaves
# nonexistent relative paths untouched, so 'lib' and './lib' compare unequal.
is_relative <- function(path) {
  xfun::is_rel_path(path)
}
