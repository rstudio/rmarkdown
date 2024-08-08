test_that("Absolute path for image from output directory are made relative to output directory", {
  skip_if_not_pandoc("2.0")
  skip_if_not_installed("xml2")
  skip_if_not_installed("withr")
  rmd <- local_rmd_file(c(
    "---",
    "  title: test",
    "---",
    "",
    "```{r}",
    "#| label: veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylongfilename",
    "plot(mtcars)",
    "```"
  ))
  # Forcing absolute path for input and output to trigger absolute path for images
  out <- rmarkdown::render(rmd, "html_document", output_options = list(self_contained = FALSE), output_dir = dirname(rmd), quiet = TRUE)
  html <- xml2::read_html(out)
  img <- xml2::xml_find_first(html, ".//img")
  # is the path correctly made relative to _files ?
  expect_match(xml2::xml_attr(img, "src"), sprintf("^%s", knitr_files_dir(basename(rmd))))
})


test_that("Absolute path from output_dir from img tag created through pandoc are made relative to output_dir", {
  skip_if_not_pandoc("2.0")
  skip_if_not_installed("xml2")
  skip_if_not_installed("withr")
  # from https://github.com/r-lib/pkgdown/issues/2341
  # Issue happens because
  # - knitr::hook_plot_md_base() will use markdown syntax when no width and height are provided (+ other contextt). Pandoc will convert and add a linebreak
  #   due to its wrapping config --wrap=auto from pandoc 2.17 which could break our processing
  # - This won't be the case for .png file for example, as a out.width is computed by knitr internally which trigger <img src=> tag use
  # - This is why we recreate using gif below
  #   1. Take the gif from package
  #   2. Copy it to the fig_path() which will be in output directory
  #   3. Use include_graphics to include it in knitr, keeping absolute path
  rmd <- local_rmd_file(c(
    "---",
    "  title: test",
    "---",
    "",
    "```{r}",
    "#| label: veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylongfilename",
    "gif <- system.file('rmd/h/rmarkdown/rmd_loader.gif', package = 'rmarkdown')",
    "stopifnot(file.exists(gif))",
    "file <- knitr::fig_path('.gif')",
    "dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)",
    "file.copy(gif, file)",
    "knitr::include_graphics(file, rel_path = FALSE)",
    "```"
  ))
  out <- rmarkdown::render(rmd, "html_document", output_options = list(self_contained = FALSE, keep_md = TRUE), output_dir = dirname(rmd), quiet = TRUE)
  html <- xml2::read_html(out)
  img <- xml2::xml_find_first(html, ".//img")
  # # is the path correctly made relative to _files ?
  expect_match(xml2::xml_attr(img, "src"), sprintf("^%s", knitr_files_dir(basename(rmd))))
})
