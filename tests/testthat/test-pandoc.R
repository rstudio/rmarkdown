# TODO: to remove when switching the package to edition 3
local_edition(3)

test_that("build highlight args for pandoc correctly", {
  hl_style <- function(name) c("--highlight-style", name)
  expect_equal(pandoc_highlight_args(NULL), "--no-highlight")
  expect_equal(pandoc_highlight_args("default"), hl_style("tango"))
  expect_equal(pandoc_highlight_args("default", "zenburn"), hl_style("zenburn"))
  expect_equal(pandoc_highlight_args("zenburn"), hl_style("zenburn"))
})

test_that("Detect if a theme file is providing in highlight", {
  expect_equal(resolve_highlight("default"), "default")
  expect_equal(resolve_highlight("breezedark"), "breezedark")
  expect_equal(resolve_highlight("breez"), "breezedark")
  expect_error(resolve_highlight("textmate"), "must be one of")
  expect_equal(
    resolve_highlight("textmate", html_highlighters()), "textmate"
  )
  if (pandoc_available("2.0")) {
    expect_equal(resolve_highlight("arrow"), pkg_file_highlight("arrow.theme"))
    expect_equal(resolve_highlight("custom.theme"), "custom.theme")
    expect_error(resolve_highlight("custom.json"), "a file with extension")
  } else {
    expect_error(resolve_highlight("arrow"), "requires Pandoc 2.0")
  }
})

test_that("Correct HTML highlighting argument as requested", {
  # helpers
  hl_args <- function(highlight, template, downlit = FALSE) {
    pandoc_html_highlight_args(template = template, highlight = highlight,
                               highlight_downlit = downlit)
  }
  hl_style <- pandoc_highlight_args
  downlit  <- pandoc_variable_arg("highlight-downlit")
  highlightjs <- pandoc_variable_arg("highlightjs", "1")
  arrow_theme <- hl_style(pkg_file_highlight("arrow.theme"))
  rstudio_theme <- hl_style(pkg_file_highlight("rstudio.theme"))
  # check logic
  # no engine
  expect_equal(hl_args(NULL, "default"), hl_style(NULL))
  expect_equal(hl_args(NULL, "dummy.html"), hl_style(NULL))
  # pandoc
  expect_equal(hl_args("default", "dummy.html"), hl_style("pygments"))
  expect_equal(hl_args("zenburn", "dummy.html"), hl_style("zenburn"))
  expect_equal(hl_args("breezedark", "default"), hl_style("breezedark"))
  # highlight
  expect_equal(hl_args("default", "default"), c(hl_style(NULL), highlightjs))
  expect_equal(hl_args("textmate", "default"), c(hl_style(NULL), highlightjs))
  expect_error(hl_args("textmate", "dummy.html"), "Pandoc engine")
  # downlit
  expect_equal(hl_args("tango", "default", TRUE), c(hl_style("tango"), downlit))
  expect_equal(hl_args("tango", "dummy.html", TRUE), c(hl_style("tango"), downlit))
  expect_error(hl_args("textmate", "dummy.html", TRUE), "downlit engine")
  expect_error(hl_args("textmate", "default", TRUE), "downlit engine")
  # custom theme
  skip_if_not(pandoc2.0())
  expect_equal(hl_args("default", "default", TRUE), c(arrow_theme, downlit))
  expect_equal(hl_args("default", "dummy.html", TRUE), c(arrow_theme, downlit))
  expect_equal(hl_args("arrow", "default", FALSE), c(arrow_theme))
  expect_equal(hl_args("arrow", "dummy.html", FALSE), c(arrow_theme))
  expect_equal(hl_args("rstudio", "default", TRUE), c(rstudio_theme, downlit))
  expect_equal(hl_args("rstudio", "dummy.html", TRUE), c(rstudio_theme, downlit))
  expect_equal(hl_args("path/to.theme", "default"), c(hl_style("path/to.theme")))
  expect_equal(hl_args("path/to.theme", "dummy.html"), c(hl_style("path/to.theme")))
})

test_that("detect highlightjs theme", {
  expect_false(is_highlightjs(NULL))
  expect_false(is_highlightjs("zenburn"))
  expect_false(is_highlightjs("path/to/hl.theme"))

  expect_true(is_highlightjs("default"))
  expect_true(is_highlightjs("textmate"))
})

test_that("Converting bib file is working", {
  skip_on_cran()
  skip_if_not_pandoc("2.11") # only test with newer Pandoc citeproc
  skip_on_os("windows") # UTF-8 and windows does not work well for now.
  bib_file <- test_path("resources/UTF8.bib")
  expect_snapshot_value(pandoc_citeproc_convert(bib_file, "list"), style = "deparse")
  expect_snapshot_output(pandoc_citeproc_convert(bib_file, "json"))
  expect_snapshot_output(pandoc_citeproc_convert(bib_file, "yaml"))
})

test_that("pandoc_math_args() build correct CLI flag", {
  expect_identical(pandoc_math_args("katex"), c("--katex"))
  expect_identical(pandoc_math_args("webtex", "url"), c("--webtex=url"))
  expect_error(pandoc_math_args("gladtex", "CDN"), "gladtex does not support")
})
