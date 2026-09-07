# Guard against emitting Pandoc command-line arguments that Pandoc has
# deprecated. Deprecated args only trigger a warning (not an error), so they
# slip past the daily nightly-Pandoc CI job silently (#2638). This test runs
# the real Pandoc binary and fails if it prints a "Deprecated" warning for any
# argument we generate, giving us advance notice before an arg is removed.

# Run pandoc with `args` on a small math document and return its stderr.
pandoc_stderr <- function(args) {
  input <- withr::local_tempfile(fileext = ".md")
  xfun::write_utf8("$e = mc^2$", input)
  suppressWarnings(system2(
    pandoc(),
    c(shQuote(input), "-t", "html", args),
    stdout = FALSE, stderr = TRUE
  ))
}

test_that("math_method args are not deprecated by Pandoc", {
  skip_if_not_pandoc()
  skip_on_cran()
  for (engine in pandoc_math_engines()) {
    err <- pandoc_stderr(pandoc_math_args(engine))
    expect_no_match(
      err, "Deprecated",
      info = sprintf("Pandoc reports a deprecated arg for engine '%s': %s",
                     engine, paste(err, collapse = " "))
    )
  }
})

test_that("self_contained args are not deprecated by Pandoc", {
  skip_if_not_pandoc()
  skip_on_cran()
  err <- pandoc_stderr(self_contained_args())
  expect_no_match(err, "Deprecated", info = paste(err, collapse = " "))
})
