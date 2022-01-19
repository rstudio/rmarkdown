test_that("available_templates() correctly returns template", {
  expect_identical(
    expect_invisible(available_templates("donotexist")),
    character()
  )
  expect_identical(available_templates(), dir(pkg_file("rmarkdown/templates")))
  expect_identical(available_templates(full_path = TRUE),
                   dir(pkg_file("rmarkdown/templates"), full.names = TRUE))
})

test_that("draft() correctly creates template from a package", {
  rmd <- withr::local_tempfile(fileext = ".Rmd")
  path <- expect_invisible(draft(rmd, "github_document", "rmarkdown", edit = FALSE))
  expect_identical(rmd, path)
  expect_true(file.exists(rmd))
  expect_match(xfun::read_utf8(rmd), "github_document", all = FALSE)
})

test_that("draft() correctly creates template from a path", {
  # No template
  template_dir <- withr::local_tempdir("template")
  rmd <- withr::local_tempfile(fileext = ".Rmd")
  expect_error(draft(rmd, template_dir, edit = FALSE), "No template\\.yaml")
  yaml <- file.path(template_dir, "template.yaml")

  # Incorrect yaml
  xfun::write_utf8(c("dummy: value"), yaml)
  expect_error(draft(rmd, template_dir, edit = FALSE), "template\\.yaml must contain")

  # Correct template folder with skeleton
  xfun::write_utf8(c("name: dummy", "description: for test"), yaml)
  dir.create(file.path(template_dir, "skeleton"))
  rmd_template <- file.path(template_dir, "skeleton", "skeleton.Rmd")
  xfun::write_utf8(c("# dummy"), rmd_template)
  draft(rmd, template_dir, edit = FALSE)
  expect_true(file.exists(rmd))
  expect_match(xfun::read_utf8(rmd), "# dummy")
})

test_that("draft() correctly creates a folder from a template", {
  rmd <- withr::local_tempfile(fileext = ".Rmd")
  path <- draft(rmd, "github_document", "rmarkdown", create_dir = TRUE, edit = FALSE)
  expect_identical(basename(rmd), basename(path))
  expect_identical(xfun::sans_ext(basename(rmd)), basename(dirname(path)))
  expect_true(file.exists(path))
  expect_match(xfun::read_utf8(path), "github_document", all = FALSE)
})
