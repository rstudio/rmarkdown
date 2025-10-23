context("html-uptree-lib_dir")

# copy part of our demo site to a tempdir
local_create_site <- function(files, env = parent.frame()) {
  site_dir <- tempfile()
  dir.create(site_dir, recursive = TRUE)
  withr::defer(unlink(site_dir, recursive = TRUE), envir = env)
  # by default copy all files
  if (missing(files)) files <- c("index.Rmd", "node-01", "node-02", "lib")
  file.copy(test_path("html-uptree-lib_dir", files), site_dir, recursive = TRUE)
  site_dir
}

test_that("render_pages", {

  # copy our demo site to a tempdir
  site_dir <- local_create_site()

  # render it
  capture.output({
    render(file.path(site_dir, "index.Rmd"))
    render(file.path(site_dir, "node-01", "index.Rmd"))
    render(file.path(site_dir, "node-02", "index.Rmd"))
  })

  # did the html files get rendered and the css get copied?
  html_files <- c("index.html")
  html_files <- c(file.path(site_dir, html_files),
                  file.path(site_dir, c("node-01", "node-02"), html_files))
  expect_true(all(file.exists(html_files)))
})

test_that("disable-uptree-lib_dir", {
  site_dir <- local_create_site(c("index.Rmd", "node-03", "lib"))
  expect_error(
    render(file.path(site_dir, "node-03", "index.Rmd")),
    regexp = "The path .* does not appear to be a descendant of .*"
  )
})

