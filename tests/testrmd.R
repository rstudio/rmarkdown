# only run this test on CI platforms
if (.Platform$OS.type == 'unix' && !is.na(Sys.getenv('CI', NA))) {

  # test if Word documents can be rendered with a specified intermediate dir
  # (https://github.com/rstudio/rmarkdown/issues/1431)
  system2(rmarkdown::pandoc_exec(), '--print-default-data-file reference.docx', stdout = 'rmd/template.docx')
  rmarkdown::render('rmd/word.Rmd', intermediates_dir = 'tmp')

}
