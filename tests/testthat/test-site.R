context("site")

test_that("render_site", {

  skip_on_cran()

  # copy our demo site to a tempdir
  site_dir <- tempfile()
  dir.create(site_dir)
  files <- c("_site.yml", "index.Rmd", "PageA.Rmd",
             "PageB.rmd", "PageC.md", "styles.css",
             "script.R", "docs.txt")
  file.copy(file.path("site", files), site_dir, recursive = TRUE)

  # render it
  capture.output(render_site(site_dir))

  # did the html files get rendered and the css get copied?
  html_files <- c("index.html", "PageA.html", "PageB.html", "PageC.html")
  html_files <- file.path(site_dir, "_site", html_files)
  expect_true(all(file.exists(html_files)))

  # moved directories
  moved <- c("site_libs", "PageA_files")
  expect_true(all(!file.exists(file.path(site_dir, moved))))
  expect_true(all(file.exists(file.path(site_dir, "_site", moved))))

  # respected includes
  included <- "script.R"
  expect_true(all(file.exists(file.path(site_dir, "_site", included))))

  # respected excluded
  excluded <- "docs.txt"
  expect_true(all(!file.exists(file.path(site_dir, "_site", excluded))))
})
