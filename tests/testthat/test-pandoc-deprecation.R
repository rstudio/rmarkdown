# Guard against emitting Pandoc command-line arguments that Pandoc has
# deprecated. Deprecated args only trigger a warning on stderr (not an error),
# so they slip past the daily nightly-Pandoc CI job silently (#2638).
#
# pandoc_convert() captures Pandoc's stderr and turns any "Deprecated: ..."
# line into an R warning, so we can catch deprecations generically -- without
# enumerating the deprecated arguments one by one -- by rendering documents and
# asserting no such warning is raised.

test_that("warn_if_pandoc_deprecated() parses Pandoc's stderr generically", {
  # nothing to warn about
  expect_silent(warn_if_pandoc_deprecated(character()))
  expect_silent(warn_if_pandoc_deprecated(c("[INFO] all good", "done")))
  # a single deprecation, with the "[WARNING] " prefix stripped
  expect_warning(
    warn_if_pandoc_deprecated(
      "[WARNING] Deprecated: --mathjax. Use --math-method=mathjax[:URL] instead."
    ),
    "Deprecated: --mathjax\\. Use --math-method"
  )
  # case-insensitive, and multiple deprecations reported together
  expect_warning(
    warn_if_pandoc_deprecated(c(
      "[WARNING] Deprecated: --foo.",
      "[WARNING] deprecated: --bar."
    )),
    "--foo.*--bar"
  )
})

# Render a minimal document with math to the given format and collect any
# warning message raised by pandoc_convert().
render_warning <- function(output_format, ...) {
  input <- local_rmd_file("---", "title: t", "---", "", "$e = mc^2$", "")
  output <- withr::local_tempfile()
  msg <- NULL
  withCallingHandlers(
    rmarkdown::render(
      input, output_format = output_format, output_file = output,
      quiet = TRUE, ...
    ),
    warning = function(w) {
      msg <<- c(msg, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  msg
}

test_that("built-in HTML formats do not emit deprecated Pandoc args", {
  skip_if_not_pandoc()
  skip_on_cran()
  formats <- list(
    html_document(),
    html_document(math_method = "katex"),
    html_document(math_method = "webtex"),
    html_document(math_method = "mathml"),
    html_fragment(),
    html_vignette()
  )
  for (fmt in formats) {
    msg <- render_warning(fmt)
    expect_false(
      any(grepl("Deprecated", msg %||% "")),
      info = paste(msg, collapse = " ")
    )
  }
})
