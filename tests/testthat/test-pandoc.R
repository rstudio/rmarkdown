test_that("Correct syntax highlighting argument as requested", {
  # helpers
  hl_args <- function(highlight, template, downlit = FALSE) {
    pandoc_html_highlight_args(template = template, highlight = highlight,
                               highlight_downlit = downlit)
  }
  hl_style <- function(name = NULL) {
    if (is.null(name))
      "--no-highlight"
    else
      c("--highlight-style", name)
  }
  downlit  <- pandoc_variable_arg("highlight-downlit=1")
  highlightjs <- pandoc_variable_arg("highlightjs=1")
  a11y_theme <- hl_style(pkg_file_highlight("a11y.theme"))
  # check logic
  expect_equal(hl_args(NULL, "default"), hl_style())
  expect_equal(hl_args(NULL, "dummy.html"), hl_style())
  expect_equal(hl_args("default", "dummy.html"), hl_style("pygments"))
  expect_equal(hl_args("zenburn", "dummy.html"), hl_style("zenburn"))
  expect_equal(hl_args("breezedark", "default"), hl_style("breezedark"))
  expect_equal(hl_args("default", "default"), c(hl_style(), highlightjs))
  expect_equal(hl_args("default", "default", TRUE), c(a11y_theme, downlit))
  expect_equal(hl_args("default", "dummy.html", TRUE), c(a11y_theme, downlit))
  expect_equal(hl_args("textmate", "dummy.html", TRUE), c(a11y_theme, downlit))
  expect_equal(hl_args("tango", "default", TRUE), c(hl_style("tango"), downlit))
})
