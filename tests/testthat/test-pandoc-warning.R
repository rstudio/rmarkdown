# When RMARKDOWN_PANDOC_ERROR_ON_WARNING is set, pandoc_convert() aborts on any
# Pandoc "[WARNING] ..." message (#2638).

test_that("stop_on_pandoc_warning() parses Pandoc's stderr generically", {
  # nothing to abort on
  expect_silent(stop_on_pandoc_warning(character()))
  expect_silent(stop_on_pandoc_warning(c("[INFO] all good", "done")))
  # a single warning, with the "[WARNING] " prefix stripped
  expect_error(
    stop_on_pandoc_warning(
      "[WARNING] Deprecated: --mathjax. Use --math-method=mathjax[:URL] instead."
    ),
    "Deprecated: --mathjax\\. Use --math-method"
  )
  # multiple warnings of any kind reported together
  expect_error(
    stop_on_pandoc_warning(c(
      "[WARNING] Deprecated: --foo.",
      "[WARNING] Could not fetch resource bar."
    )),
    "--foo.*bar"
  )
})

test_that("pandoc_convert() aborts on Pandoc warnings only when enabled", {
  skip_if_not_pandoc()
  skip_on_cran()

  # two headers with the same explicit identifier make Pandoc emit a
  # "[WARNING] Duplicate identifier" on all supported versions without failing
  # (auto-generated identifiers would be de-duplicated silently instead)
  input <- withr::local_tempfile(fileext = ".md")
  xfun::write_utf8(c("# A {#dup}", "", "# B {#dup}"), input)
  output <- withr::local_tempfile(fileext = ".html")
  convert <- function() {
    pandoc_convert(input, to = "html", output = output)
  }

  # disabled by default: the warning does not abort the conversion
  withr::with_envvar(c(RMARKDOWN_PANDOC_ERROR_ON_WARNING = "false"), {
    expect_no_error(convert())
  })

  # enabled: Pandoc's warning aborts the conversion (so R CMD check fails)
  withr::with_envvar(c(RMARKDOWN_PANDOC_ERROR_ON_WARNING = "true"), {
    expect_error(convert(), "[Dd]uplicate")
  })
})
