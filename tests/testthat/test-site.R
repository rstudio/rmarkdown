context("site")

test_that("render_site renders all pages in a directory", {

  skip_on_cran()

  # copy our demo site to a tempdir
  site_dir <- tempfile()
  dir.create(site_dir)
  file.copy(c("site/index.Rmd", "site/PageA.Rmd", "site/PageB.Rmd"),
            site_dir, recursive = TRUE)

  # render it
  capture.output(render_site(site_dir))

  # did the html files get rendered?
  expect_true(file.exists(file.path(site_dir, "index.html")))
  expect_true(file.exists(file.path(site_dir, "PageA.html")))
  expect_true(file.exists(file.path(site_dir, "PageB.html")))
})
