test_that("Correct syntax highlighting argument as requested", {
  # helpers
  highlight_style <- function(name = NULL) {
    if (is.null(name))
      "--no-highlight"
    else
      c("--highlight-style", name)
  }
  downlit  <- pandoc_variable_arg("highlight-downlit=1")
  highlightjs <- pandoc_variable_arg("highlightjs=1")
  a11y_theme <- highlight_style(pkg_file_highlight("a11y.theme"))
  # check logic
  expect_equal(pandoc_html_highlight_args("default", NULL), highlight_style())
  expect_equal(pandoc_html_highlight_args("dummy.html", NULL), highlight_style())
  expect_equal(pandoc_html_highlight_args("dummy.html", "default"),
               highlight_style("pygments"))
  expect_equal(pandoc_html_highlight_args("dummy.html", "zenburn"),
               highlight_style("zenburn"))
  expect_equal(pandoc_html_highlight_args("default", "breezedark"),
               highlight_style("breezedark"))
  expect_equal(pandoc_html_highlight_args("default", "default"),
               c(highlight_style(), highlightjs))
  expect_equal(pandoc_html_highlight_args("default", "default",
                                          highlight_downlit = TRUE),
               c(a11y_theme, downlit))
  expect_equal(pandoc_html_highlight_args("dummy.html", "default",
                                          highlight_downlit = TRUE),
               c(a11y_theme, downlit))
  expect_equal(pandoc_html_highlight_args("dummy.html", "textmate",
                                          highlight_downlit = TRUE),
               c(a11y_theme, downlit))
  expect_equal(pandoc_html_highlight_args("default", "tango",
                                          highlight_downlit = TRUE),
               c(highlight_style("tango"),downlit))
})
