# TODO: to remove when switching the package to edition 3
local_edition(3)

test_that("dependency merge is correct", {
  # normalize lists for comparison by removing names and NULL elements
  prepare_list <- function(lst) {
    names(lst) <- NULL
    lapply(lst, function(item) {
      item[!sapply(item, is.null)]
    })
  }

  # tests a dependency merge
  test_dep_merge <- function(input, output, doeswarn = FALSE) {
    deps <- flatten_html_dependencies(input)
    expect_warning(
      result <- html_dependency_resolver(deps),
      if (doeswarn) NULL else NA
    )
    result <- prepare_list(result)
    output <- prepare_list(output)
    expect_identical(result, output)
  }

  # identity
  test_dep_merge(
    # input
    list(
      htmlDependency(
        name = "foo",
        version = "1.1.0",
        src = pkg_file("rmd/h"),
        script = "foo.js")),
    # output
    list(
      htmlDependency(
        name = "foo",
        version = "1.1.0",
        src = pkg_file("rmd/h"),
        script = "foo.js")))

  # don't replace a higher version with a lower one
  test_dep_merge(
    # input
    list(
      htmlDependency(
        name = "foo",
        version = "1.2.0",
        src = pkg_file("rmd/h"),
        script = "foo.js"),
      htmlDependency(
        name = "foo",
        version = "1.1.0",
        src = pkg_file("rmd/h"),
        script = "foo.js")),
    # output
    list(
      htmlDependency(
        name = "foo",
        version = "1.2.0",
        src = pkg_file("rmd/h"),
        script = "foo.js")))

  # preserve dependency order on replacement
  test_dep_merge(
    # input
    list(
      htmlDependency(
        name = "foo",
        version = "1.1.0",
        src = pkg_file("rmd/h"),
        script = "foo.js"),
      htmlDependency(
        name = "bar",
        version = "1.1.0",
        src = pkg_file("rmd/h"),
        script = "foo.js"),
      htmlDependency(
        name = "baz",
        version = "1.1.0",
        src = pkg_file("rmd/h"),
        script = "baz.js"),
      htmlDependency(
        name = "bar",
        version = "1.2.0",
        src = pkg_file("rmd/h"),
        script = "foo.js")),
    # output
    list(
      htmlDependency(
        name = "foo",
        version = "1.1.0",
        src = pkg_file("rmd/h"),
        script = "foo.js"),
      htmlDependency(
        name = "bar",
        version = "1.2.0",
        src = pkg_file("rmd/h"),
        script = "foo.js"),
      htmlDependency(
        name = "baz",
        version = "1.1.0",
        src = pkg_file("rmd/h"),
        script = "baz.js")))

  # support nested dependency lists
  test_dep_merge(
    # input
    list(
      htmlDependency(
        name = "bar",
        version = "1.1.0",
        src = pkg_file("rmd/h"),
        script = "foo.js"),
      list(
        htmlDependency(
          name = "baz",
          version = "1.1.0",
          src = pkg_file("rmd/h"),
          script = "baz.js"),
        htmlDependency(
          name = "bar",
          version = "1.2.0",
          src = pkg_file("rmd/h"),
          script = "foo.js"))),
    # output
    list(
      htmlDependency(
        name = "bar",
        version = "1.2.0",
        src = pkg_file("rmd/h"),
        script = "foo.js"),
      htmlDependency(
        name = "baz",
        version = "1.1.0",
        src = pkg_file("rmd/h"),
        script = "baz.js")))

  # ignore knit_meta information other than html_dependency
  test_dep_merge(
    # input
    list(
      structure(list(foo = "irrelevant"), class = "irrelevant"),
      list(
        htmlDependency(
          name = "baz",
          version = "1.1.0",
          src = pkg_file("rmd/h"),
          script = "baz.js"))),
    # output
    list(
      htmlDependency(
        name = "baz",
        version = "1.1.0",
        src = pkg_file("rmd/h"),
        script = "baz.js")))

})

test_that("Dependencies are correctly validated", {
  # not a html dep
  expect_error(validate_html_dependency(list(a = 1)), "is not of class html_dependency", fixed = TRUE)
  skip_if_not_installed("htmltools")
  # file based dep
  dep <- htmlDependency(name = "foo", version = "1.1.0", src = pkg_file("rmd/h"), script = "foo.js")
  expect_identical(validate_html_dependency(dep), dep)
  # href base dep
  dep <- htmlDependency(name = "foo", version = "1.1.0", src = c(href = "https://example.org"), script = "foo.js")
  expect_identical(validate_html_dependency(dep), dep)
  # incomplete html deps
  dep2 <- dep; dep2$name <- NULL
  expect_error(validate_html_dependency(dep2), "name .* not provided")
  dep2 <- dep; dep2$version <- NULL
  expect_error(validate_html_dependency(dep2), "version .* not provided")
  dep2 <- dep; dep2$src <- NULL
  expect_error(validate_html_dependency(dep2), "src .* not provided")
  dep2 <- dep; dep2$src <- list(file = tempfile("donotexist"))
  expect_error(validate_html_dependency(dep2), "path for html_dependency not found:", fixed = TRUE)
})

test_that("html_dependencies_as_string tranforms correctly", {
  deps <- list(
    htmlDependency(name = "bar", version = "1.2.0", src = pkg_file("rmd/h"), script = "foo.js"),
    htmlDependency(name = "bar", version = "1.2.0", src = c(href = "https://example.org/"), script = "foo.js"),
    htmlDependency(name = "baz", version = "1.1.0", src = pkg_file("rmd/h"), script = "baz.js")
  )
  odir <- withr::local_tempdir()
  dir.create(ldir <- file.path(odir, "lib"))
  expect_snapshot(html_dependencies_as_string(deps, ldir, odir))
})

test_that("html_dependencies_fonts loads the correct fonts dep", {
  fa <- html_dependency_font_awesome()
  io <- html_dependency_ionicons()
  expect_identical(html_dependencies_fonts(TRUE, FALSE), list(fa))
  expect_identical(html_dependencies_fonts(FALSE, TRUE), list(io))
  expect_identical(html_dependencies_fonts(TRUE, TRUE), list(fa, io))
})

test_that("header-attr can be opt-out", {
  withr::local_options(list(rmarkdown.html_dependency.header_attr = FALSE))
  expect_null(html_dependency_header_attrs())
})
