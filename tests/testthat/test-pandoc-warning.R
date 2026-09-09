# Pandoc prints "[WARNING] <message>" on stderr for non-fatal issues, e.g.
# deprecated command-line arguments (#2638) or duplicate header identifiers.
# These do not fail the conversion, so they slip past the daily nightly-Pandoc
# CI job silently. When RMARKDOWN_PANDOC_WARN is set, pandoc_convert() captures
# Pandoc's stderr and turns such lines into R warnings, so we can catch any
# Pandoc warning generically -- without enumerating them one by one.

test_that("warn_if_pandoc_warning() parses Pandoc's stderr generically", {
  # nothing to warn about
  expect_silent(warn_if_pandoc_warning(character()))
  expect_silent(warn_if_pandoc_warning(c("[INFO] all good", "done")))
  # a single warning, with the "[WARNING] " prefix stripped
  expect_warning(
    warn_if_pandoc_warning(
      "[WARNING] Deprecated: --mathjax. Use --math-method=mathjax[:URL] instead."
    ),
    "Deprecated: --mathjax\\. Use --math-method"
  )
  # multiple warnings of any kind reported together
  expect_warning(
    warn_if_pandoc_warning(c(
      "[WARNING] Deprecated: --foo.",
      "[WARNING] Could not fetch resource bar."
    )),
    "--foo.*bar"
  )
})

test_that("pandoc_convert() warns on Pandoc warnings only when enabled", {
  skip_if_not_pandoc()
  skip_on_cran()

  # two headers with the same text produce a duplicate identifier, which makes
  # Pandoc emit a [WARNING] on all supported versions without failing
  input <- withr::local_tempfile(fileext = ".md")
  xfun::write_utf8(c("# Dup", "", "# Dup"), input)
  output <- withr::local_tempfile(fileext = ".html")
  convert <- function() {
    pandoc_convert(input, to = "html", output = output)
  }

  # disabled by default: no capture, no warning
  withr::with_envvar(c(RMARKDOWN_PANDOC_WARN = "false"), {
    expect_no_warning(convert())
  })

  # enabled: Pandoc's warning surfaces as an R warning
  withr::with_envvar(c(RMARKDOWN_PANDOC_WARN = "true"), {
    expect_warning(convert(), "[Dd]uplicate")
  })
})
