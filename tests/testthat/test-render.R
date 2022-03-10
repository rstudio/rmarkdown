# TODO: to remove when switching the package to edition 3
local_edition(3)

test_that("Metadata is available before pre_knit", {
  message_pre_knit = 'pre_knit handles metadata'
  fmt <- md_document()
  fmt$pre_knit <- function(input, ...) {
    # `metadata` requires `rmarkdown::`-prefix. Otherwise, it becomes `list()`.
    if (identical(rmarkdown::metadata, list(foo = 'bar'))) {
      message(message_pre_knit)
    }
  }
  input_file = tempfile(fileext = '.md')
  writeLines('---\nfoo: bar\n---', input_file)
  expect_message(render(input_file, fmt, quiet = TRUE), message_pre_knit)
})

test_that("file_scope split correctly input file", {
  skip_if_not(getRversion() >= 3.5)
  rmd <- local_rmd_file(c("# H1", "content1", "# H2", "content2"))
  file_scope_fun <- function(file) {
    x <- xfun::read_utf8(file)
    list(
      list(name = "A", content = x[1:2]),
      list(name = "B", content = x[3:4])
    )
  }
  splitted <- file_scope_split(rmd, file_scope_fun)
  expect_true(all(file.exists(splitted)))
  on.exit(unlink(splitted), add = TRUE, after = FALSE)
  expect_match(splitted, "[.]split[.]md$")
  expect_snapshot_file(splitted[1])
  expect_snapshot_file(splitted[2])
})

test_that("syntax definition file is correctly added", {
  # do nothing cases
  expect_identical(add_syntax_definition("--no-highlight"), "--no-highlight")
  expect_identical(
    add_syntax_definition(c("--no-highlight", "--other")),
    c("--no-highlight", "--other")
  )
  withr::with_options(
    list(rmarkdown.highlighting.xml.add = FALSE),
    expect_identical(add_syntax_definition(""), "")
  )
  if (!pandoc_available("2.15")) {
    expect_identical(add_syntax_definition("arg1"), "arg1")
  }

  # Add custom xml files cases
  skip_if_not_pandoc("2.15")
  args <- add_syntax_definition(NULL)
  ## Defaults files are added
  expect_length(
    args1 <- setdiff(args, "--syntax-definition"),
    length(.syntax_highlight_bundled_language())
  )
  reg <- sprintf("(%s)", paste0(.syntax_highlight_bundled_language(), collapse = "|"))
  expect_match(args1, reg)
  ## User provided file is kept
  args <- add_syntax_definition(c("arg1", pandoc_syntax_definition_args("dummy/r.xml")))
  expect_match(args, "dummy.+r[.]xml$", all = FALSE)
  expect_length(grep("r[.]xml$", args), 1L)
  ## Opt in specific
  withr::with_options(list(rmarkdown.highlighting.xml.add = "markdown"), {
      args <- add_syntax_definition(c("arg1"))
      expect_length(args, 3)
      expect_match(args[3], "markdown[.]xml$")
    }
  )
  ## Opt out specific
  withr::with_options(list(rmarkdown.highlighting.xml.remove = "r"), {
      args <- add_syntax_definition(c("arg1"))
      expect_no_match(args, "r[.]xml$")
    }
  )
})
