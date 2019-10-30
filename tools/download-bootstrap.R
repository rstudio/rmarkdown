owd <- setwd('inst/rmd/h/bootstrap/css/')
unlink('*.min.css')

local({
  r <- '^\\s*<a class="dropdown-item" href="[.]/([^/]+)/">.+</a>\\s*$'
  txt <- readLines('https://bootswatch.com')
  themes <- setdiff(gsub(r, '\\1', grep(r, txt, value = TRUE)), 'default')
  for (thm in themes) {
    xfun::download_file(
      sprintf('https://bootswatch.com/4/%s/bootstrap.min.css', thm),
      paste0(thm, '.min.css')
    )
  }
})

cdn <- function(...) paste(
  c('https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/', ...), collapse = ''
)

xfun::download_file(cdn('css/bootstrap.min.css'))
# bootstrap.js + popper.js
xfun::download_file(cdn('js/bootstrap.bundle.min.js'), '../js/bootstrap.min.js')

setwd(owd)
