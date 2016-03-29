context("site")

test_that("render_site renders all pages in a directory", {

  skip_on_cran()

  # copy our demo site to a tempdir
  site_dir <- tempfile()
  dir.create(site_dir)
  file.copy(c("site/_site.yml", "site/index.md", "site/PageA.Rmd",
              "site/PageB.Rmd", "site/PageC.md", "site/styles.css"),
            site_dir, recursive = TRUE)

  # render it
  capture.output(render_site(site_dir))

  # did the html files get rendered?
  expect_true(file.exists(file.path(site_dir, "_site", "index.html")))
  expect_true(file.exists(file.path(site_dir, "_site", "PageA.html")))
  expect_true(file.exists(file.path(site_dir, "_site", "PageB.html")))
  expect_true(file.exists(file.path(site_dir, "_site", "PageC.html")))
  expect_true(file.exists(file.path(site_dir, "_site", "styles.css")))
})
