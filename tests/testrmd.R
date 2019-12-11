# only run this test on CI platforms
if (.Platform$OS.type == 'unix' && !is.na(Sys.getenv('CI', NA))) {

  # test if Word documents can be rendered with a specified intermediate dir
  # (https://github.com/rstudio/rmarkdown/issues/1431)
  system2(rmarkdown::pandoc_exec(), '--print-default-data-file reference.docx', stdout = 'rmd/template.docx')
  rmarkdown::render('rmd/word.Rmd', intermediates_dir = 'tmp')

  # bib files should be copied to the intermediates dir:
  # https://github.com/rstudio/rmarkdown/issues/1358
  rmarkdown::render('rmd/two-bibs.Rmd', intermediates_dir = tempdir())

  # use an absolute output_dir should not trigger a file.rename() warning
  # (https://github.com/rstudio/rmarkdown/issues/1224)
  withCallingHandlers(
    rmarkdown::render(
      'rmd/output_dir.Rmd', output_dir = file.path(tempdir(), 'output'),
      intermediates_dir = file.path(tempdir(), 'tmp')
    ), warning = function(e) {
      if (identical(deparse(e$call), 'file.rename(from, to)') || grepl('cannot rename file', e$message)) {
        stop(e)
      }
    }
  )

  # the default LaTeX template should work with multiple authors:
  # https://github.com/rstudio/rmarkdown/issues/1716
  rmarkdown::render('rmd/two-authors.Rmd')

  # R code in YAML should be evaluated before YAML is passed to Pandoc:
  # https://github.com/rstudio/rmarkdown/issues/1709
  rmarkdown::render('rmd/yaml-r-code.Rmd')

  # raw blocks in header-includes should work:
  # https://github.com/rstudio/rmarkdown/issues/1729
  rmarkdown::render('rmd/raw-header.Rmd')
}
