context("dependencies")

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
    expect_that(
        result <- html_dependencies_for_document(input),
        if (doeswarn) gives_warning() else not(gives_warning()))
    result <- prepare_list(result)
    output <- prepare_list(output)
    expect_identical(result, output)
  }

  # identity
  test_dep_merge(
    # input
    list(
      html_dependency(
        name = "foo",
        version = "1.1.0",
        path = rmarkdown_system_file("rmd/h"),
        script = "foo.js")),
    # output
    list(
      html_dependency(
        name = "foo",
        version = "1.1.0",
        path = rmarkdown_system_file("rmd/h"),
        script = "foo.js")))

  # don't replace a higher version with a lower one
  test_dep_merge(
    # input
    list(
      html_dependency(
        name = "foo",
        version = "1.2.0",
        path = rmarkdown_system_file("rmd/h"),
        script = "foo.js"),
      html_dependency(
        name = "foo",
        version = "1.1.0",
        path = rmarkdown_system_file("rmd/h"),
        script = "foo.js")),
    # output
    list(
      html_dependency(
        name = "foo",
        version = "1.2.0",
        path = rmarkdown_system_file("rmd/h"),
        script = "foo.js")))

  # preserve dependency order on replacement
  test_dep_merge(
    # input
    list(
      html_dependency(
        name = "foo",
        version = "1.1.0",
        path = rmarkdown_system_file("rmd/h"),
        script = "foo.js"),
      html_dependency(
        name = "bar",
        version = "1.1.0",
        path = rmarkdown_system_file("rmd/h"),
        script = "foo.js"),
      html_dependency(
        name = "baz",
        version = "1.1.0",
        path = rmarkdown_system_file("rmd/h"),
        script = "baz.js"),
      html_dependency(
        name = "bar",
        version = "1.2.0",
        path = rmarkdown_system_file("rmd/h"),
        script = "foo.js")),
    # output
    list(
    html_dependency(
      name = "foo",
      version = "1.1.0",
      path = rmarkdown_system_file("rmd/h"),
      script = "foo.js"),
    html_dependency(
      name = "bar",
      version = "1.2.0",
      path = rmarkdown_system_file("rmd/h"),
      script = "foo.js"),
    html_dependency(
      name = "baz",
      version = "1.1.0",
      path = rmarkdown_system_file("rmd/h"),
      script = "baz.js")))

  # external libraries win even when older, should emit a warning
  test_dep_merge(
    # input
    list(
      html_dependency(
        name = "foo",
        version = "1.1.0",
        path = rmarkdown_system_file("rmd/h"),
        script = "foo.js",
        external = TRUE),
      html_dependency(
        name = "foo",
        version = "1.2.0",
        path = rmarkdown_system_file("rmd/h"),
        script = "foo.js")),
    # output
    list(
      html_dependency(
        name = "foo",
        version = "1.1.0",
        path = rmarkdown_system_file("rmd/h"),
        script = "foo.js",
        external = TRUE)),
    # emits a warning
    TRUE)

  # external libraries win over older dependencies (and we should use the
  # external version)
  test_dep_merge(
    # input
    list(
      html_dependency(
        name = "foo",
        version = "1.1.0",
        path = rmarkdown_system_file("rmd/h"),
        script = "foo.js"),
      html_dependency(
        name = "foo",
        version = "1.2.0",
        path = rmarkdown_system_file("rmd/h"),
        script = "foo.js",
        external = TRUE)),
    # output
    list(
      html_dependency(
        name = "foo",
        version = "1.2.0",
        path = rmarkdown_system_file("rmd/h"),
        script = "foo.js",
        external = TRUE)))

  # support nested dependency lists
  test_dep_merge(
    # input
    list(
      html_dependency(
        name = "bar",
        version = "1.1.0",
        path = rmarkdown_system_file("rmd/h"),
        script = "foo.js"),
      list(
        html_dependency(
          name = "baz",
          version = "1.1.0",
          path = rmarkdown_system_file("rmd/h"),
          script = "baz.js"),
        html_dependency(
          name = "bar",
          version = "1.2.0",
          path = rmarkdown_system_file("rmd/h"),
          script = "foo.js"))),
    # output
    list(
      html_dependency(
        name = "bar",
        version = "1.2.0",
        path = rmarkdown_system_file("rmd/h"),
        script = "foo.js"),
      html_dependency(
        name = "baz",
        version = "1.1.0",
        path = rmarkdown_system_file("rmd/h"),
        script = "baz.js")))

  # ignore knit_meta information other than html_dependency
  test_dep_merge(
    # input
    list(
      structure(list(foo = "irrelevant"), class = "irrelevant"),
      list(
        html_dependency(
          name = "baz",
          version = "1.1.0",
          path = rmarkdown_system_file("rmd/h"),
          script = "baz.js"))),
    # output
    list(
      html_dependency(
        name = "baz",
        version = "1.1.0",
        path = rmarkdown_system_file("rmd/h"),
        script = "baz.js")))

  })
