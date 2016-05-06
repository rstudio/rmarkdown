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
        src = rmarkdown_system_file("rmd/h"),
        script = "foo.js")),
    # output
    list(
      htmlDependency(
        name = "foo",
        version = "1.1.0",
        src = rmarkdown_system_file("rmd/h"),
        script = "foo.js")))

  # don't replace a higher version with a lower one
  test_dep_merge(
    # input
    list(
      htmlDependency(
        name = "foo",
        version = "1.2.0",
        src = rmarkdown_system_file("rmd/h"),
        script = "foo.js"),
      htmlDependency(
        name = "foo",
        version = "1.1.0",
        src = rmarkdown_system_file("rmd/h"),
        script = "foo.js")),
    # output
    list(
      htmlDependency(
        name = "foo",
        version = "1.2.0",
        src = rmarkdown_system_file("rmd/h"),
        script = "foo.js")))

  # preserve dependency order on replacement
  test_dep_merge(
    # input
    list(
      htmlDependency(
        name = "foo",
        version = "1.1.0",
        src = rmarkdown_system_file("rmd/h"),
        script = "foo.js"),
      htmlDependency(
        name = "bar",
        version = "1.1.0",
        src = rmarkdown_system_file("rmd/h"),
        script = "foo.js"),
      htmlDependency(
        name = "baz",
        version = "1.1.0",
        src = rmarkdown_system_file("rmd/h"),
        script = "baz.js"),
      htmlDependency(
        name = "bar",
        version = "1.2.0",
        src = rmarkdown_system_file("rmd/h"),
        script = "foo.js")),
    # output
    list(
    htmlDependency(
      name = "foo",
      version = "1.1.0",
      src = rmarkdown_system_file("rmd/h"),
      script = "foo.js"),
    htmlDependency(
      name = "bar",
      version = "1.2.0",
      src = rmarkdown_system_file("rmd/h"),
      script = "foo.js"),
    htmlDependency(
      name = "baz",
      version = "1.1.0",
      src = rmarkdown_system_file("rmd/h"),
      script = "baz.js")))

  # support nested dependency lists
  test_dep_merge(
    # input
    list(
      htmlDependency(
        name = "bar",
        version = "1.1.0",
        src = rmarkdown_system_file("rmd/h"),
        script = "foo.js"),
      list(
        htmlDependency(
          name = "baz",
          version = "1.1.0",
          src = rmarkdown_system_file("rmd/h"),
          script = "baz.js"),
        htmlDependency(
          name = "bar",
          version = "1.2.0",
          src = rmarkdown_system_file("rmd/h"),
          script = "foo.js"))),
    # output
    list(
      htmlDependency(
        name = "bar",
        version = "1.2.0",
        src = rmarkdown_system_file("rmd/h"),
        script = "foo.js"),
      htmlDependency(
        name = "baz",
        version = "1.1.0",
        src = rmarkdown_system_file("rmd/h"),
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
          src = rmarkdown_system_file("rmd/h"),
          script = "baz.js"))),
    # output
    list(
      htmlDependency(
        name = "baz",
        version = "1.1.0",
        src = rmarkdown_system_file("rmd/h"),
        script = "baz.js")))

  })
