owd <- setwd('inst/rmd/h/bootstrap/css/')
unlink('*.min.css')

local({
  themes <- jsonlite::fromJSON('https://bootswatch.com/api/4.json')$themes
  for (i in seq_len(nrow(themes))) xfun::download_file(
    themes[i, 'cssMin'], paste0(tolower(themes[i, 'name']), '.min.css')
  )
})

cdn <- function(...) paste(
  c('https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/', ...), collapse = ''
)

xfun::download_file(cdn('css/bootstrap.min.css'))
# bootstrap.js + popper.js
xfun::download_file(cdn('js/bootstrap.bundle.min.js'), '../js/bootstrap.min.js')

# replace the CSS @import with actual CSS font specs so the themes can work offline
local({
  # font weights
  w <- c(
    '100' = 'Thin', '200' = 'ExtraLight', '300' = 'Light', '400' = 'Regular',
    '500' = 'Medium', '600' = 'SemiBold', '700' = 'Bold', '800' = 'ExtraBold',
    '900' = 'Black'
  )
  w <- c(w, setNames(paste(w, 'Italic'), paste0(names(w), 'italic')))
  w['400italic'] <- 'Italic'
  fonts <- list.files('fonts')
  r <- '.*@import url\\("?https://fonts[.]googleapis[.]com/css[?]family=([^")]+)"?\\);.*'
  for (f in list.files('.', '[.]min[.]css$')) {
    x <- xfun::read_utf8(f)
    s <- gsub(r, '\\1', grep(r, x, value = TRUE))
    if (length(s) == 0) next
    s <- gsub('+', ' ', s, fixed = TRUE)
    css <- unlist(lapply(unlist(strsplit(s, '|', fixed = TRUE)), function(a) {
      a <- unlist(strsplit(a, ':', fixed = TRUE))
      if (length(a) > 2) stop('Something wrong with the font spec: ', s)
      a <- c(a[1], if (length(a) == 1) '400' else unlist(strsplit(a[2], ',')))
      css <- NULL; f1 <- a[1]; f2 <- gsub(' +', '', f1)
      for (i in a[-1]) {
        # normalize 400i to 400italic
        i <- sub('^([0-9]+)i$', '\\1italic', i)
        f3 <- paste(f2, gsub(' +', '', w[i]), sep = '-')
        f4 <- sprintf('fonts/%s.ttf', f3)
        if (!file.exists(f4)) stop(
          'The font ', f4, ' does not exist. You need to download it.'
        )
        fonts <<- setdiff(fonts, basename(f4))
        css <- c(css, c(
          "@font-face{",
          sprintf("font-family:'%s';", f1),
          sprintf("font-style:%s;", if (grepl('italic', i)) 'italic' else 'normal'),
          sprintf("font-weight:%s;", sub('italic', '', i)),
          sprintf(
            "src:local('%s'), local('%s'), url(%s) format('truetype');",
            paste(f1, w[i]), f3, f4
          ),
          "}"
        ))
      }
      css
    }))
    m <- gregexpr(gsub('^[.][*]|[.][*]$', '', r), x)
    regmatches(x, m) <- list(paste(c('\n', css, '\n'), collapse = ''))
    xfun::write_utf8(x, f)
  }
  if (length(fonts) > 0) warning(
    'These fonts are no longer used: ', paste(fonts, collapse = ', ')
  )
})

setwd(owd)
