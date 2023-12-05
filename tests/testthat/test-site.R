local_edition(3)

# copy part of our demo site to a tempdir
local_create_site <- function(files, env = parent.frame()) {
  site_dir <- tempfile()
  dir.create(site_dir, recursive = TRUE)
  withr::defer(unlink(site_dir, recursive = TRUE), envir = env)
  # by default copy all files
  if (missing(files)) files <- c("_site.yml", "index.Rmd", "PageA.Rmd",
                        "PageB.rmd", "PageC.md", "PageD.R", "styles.css",
                        "script.R", "docs.txt")
  file.copy(test_path("site", files), site_dir, recursive = TRUE)
  site_dir
}

test_that("render_site works", {

  # copy our demo site to a tempdir
  site_dir <- local_create_site()

  # render it
  capture.output(render_site(site_dir))

  # did the html files get rendered and the css get copied?
  html_files <- c("index.html", "PageA.html", "PageB.html", "PageC.html",
                  "PageD.html")
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

test_that("site_generator() correctly try to use custom site generator", {
  # copy our demo site to a tempdir
  site_dir <- local_create_site()
  withr::local_dir(site_dir)
  expect_equal(site_generator(), default_site("."))
  xfun::gsub_file("index.Rmd", "(output: html_document)", "\\1\nsite: rmarkdown::dummy_site")
  # It errors if not found
  expect_error(site_generator(), regexp = "'dummy_site' is not an exported object from 'namespace:rmarkdown'", fixed = TRUE)
  file.rename("index.Rmd", "index.rmd")
  expect_error(site_generator(), regexp = "'dummy_site' is not an exported object from 'namespace:rmarkdown'", fixed = TRUE)
})

test_that("render_site respects 'new_session' in the config", {

  skip_if_not_installed("xfun", "0.13")

  # copy parts of our demo site to a tempdir
  files <- c("_site.yml", "index.Rmd", "PageA.Rmd", "PageB.rmd", "PageD.R")
  site_dir <- local_create_site(files)

  # default behaviour --> new_session: false
  render_site(site_dir, quiet = TRUE)
  a <- readLines(file.path(site_dir, "_site", "PageA.html"))
  b <- readLines(file.path(site_dir, "_site", "PageB.html"))

  # pkg loaded in PageA (tinytex) should show up in search path of PageB
  expect_match(a, "library(tinytex)", fixed = TRUE, all = FALSE)
  expect_true(any(grepl("tinytex", b, fixed = TRUE)))

  # edit config --> new_session: true
  cat("new_session: true", file = file.path(site_dir, "_site.yml"), append = TRUE)

  render_site(site_dir, quiet = TRUE)
  a <- readLines(file.path(site_dir, "_site", "PageA.html"))
  b <- readLines(file.path(site_dir, "_site", "PageB.html"))

  # pkg loaded in PageA (tinytex) should NOT show up in search path of PageB
  expect_match(a, "library(tinytex)", fixed = TRUE, all = FALSE)
  expect_false(any(grepl("tinytex", b, fixed = TRUE)))
})

test_that("clean_site gives notices before removing", {
  site_dir <- local_create_site()
  render_site(site_dir, quiet = TRUE)
  withr::local_dir(site_dir)
  expect_output(clean_site(), "removed.*_site")
  expect_true(dir.exists("_site"))
  expect_silent(clean_site(preview = FALSE, quiet = TRUE))
  expect_output(clean_site(preview = FALSE, quiet = FALSE), "Nothing")
})
