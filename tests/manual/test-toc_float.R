# to test jqueryui update

proj <- xfun::proj_root()

rmd <- file.path(proj, "tests", "manual", "test.Rmd")

html_doc <- function(...) rmarkdown::html_document(toc = TRUE, toc_float = TRUE, ...)

res <- rmarkdown::render(rmd, html_doc())
browseURL(res)
unlink(res)

rmarkdown::render(rmd, html_doc(theme = list(version = 4)))
browseURL(res)
unlink(res)

rmarkdown::render(rmd, html_doc(theme = list(version = 5)))
browseURL(res)
unlink(res)

rmarkdown::render(rmd, html_doc(theme = list(version = 5, bootswatch = "minty")))
browseURL(res)
unlink(res)
