# Pandoc prints "[WARNING] Deprecated: <arg>. ..." on stderr for deprecated
# command-line arguments. These only warn (they do not fail the conversion), so
# they slip past the daily nightly-Pandoc CI job silently (#2638). When
# RMARKDOWN_PANDOC_WARN_DEPRECATED is set, pandoc_convert() captures Pandoc's
# stderr and turns such lines into R warnings, so we can catch deprecations
# generically -- without enumerating the deprecated arguments one by one.

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

test_that("pandoc_convert() warns on deprecated args only when enabled", {
  skip_if_not_pandoc()
  skip_on_cran()

  # feed Pandoc a deprecated argument on purpose (--mathjax), but only
  # Pandoc >= 3.11 actually deprecates it
  skip_if_not_pandoc("3.11")

  input <- withr::local_tempfile(fileext = ".md")
  xfun::write_utf8("hello", input)
  output <- withr::local_tempfile(fileext = ".html")

  # disabled by default: no capture, no warning
  withr::with_envvar(c(RMARKDOWN_PANDOC_WARN_DEPRECATED = "false"), {
    expect_no_warning(
      pandoc_convert(input, to = "html", output = output, options = "--mathjax")
    )
  })

  # enabled: the deprecated --mathjax flag surfaces as an R warning
  withr::with_envvar(c(RMARKDOWN_PANDOC_WARN_DEPRECATED = "true"), {
    expect_warning(
      pandoc_convert(input, to = "html", output = output, options = "--mathjax"),
      "[Dd]eprecated"
    )
  })
})
