test_that("Correct syntax highlighting argument as requested", {
  expect_equal(pandoc_html_highlight_args("default", NULL), "--no-highlight")
  expect_equal(pandoc_html_highlight_args("dummy.html", NULL), "--no-highlight")
  expect_equal(pandoc_html_highlight_args("dummy.html", "default"),
               c("--highlight-style", "pygments"))
  expect_equal(pandoc_html_highlight_args("dummy.html", "zenburn"),
               c("--highlight-style", "zenburn"))
  expect_equal(pandoc_html_highlight_args("default", "breezedark"),
               c("--highlight-style", "breezedark"))
  expect_equal(pandoc_html_highlight_args("default", "default"),
               c("--no-highlight", "--variable", "highlightjs=1"))
  expect_equal(pandoc_html_highlight_args("default", "default",
                                          highlight_downlit = TRUE),
               c("--highlight-style", "pygments"))
  expect_equal(pandoc_html_highlight_args("dummy.html", "default",
                                          highlight_downlit = TRUE),
               c("--highlight-style", "pygments"))
  expect_equal(pandoc_html_highlight_args("default", "tango",
                                          highlight_downlit = TRUE),
               c("--highlight-style", "tango"))
})
