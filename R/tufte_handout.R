
#' Tufte handout formats (PDF and HTML)
#' 
#' Templates for creating handouts according to the style of Edward R. Tufte and
#' Richard Feynman.
#' 
#' \code{tufte_handout()} provides the PDF format based on the Tufte-LaTeX 
#' class: \url{https://tufte-latex.github.io/tufte-latex/}.
#' @inheritParams pdf_document
#' @param ... Other arguments to be passed to \code{\link{pdf_document}} or 
#'   \code{\link{html_document}} (note you cannot use the \code{template} 
#'   argument in \code{tufte_handout} or the \code{theme} argument in 
#'   \code{tufte_html()}; these arguments have been set internally)
#'   
#' @export
tufte_handout <- function(
  fig_width = 4, fig_height = 2.5, fig_crop = TRUE, dev = "pdf",
  highlight = "default", ...
) {
  
  # resolve default highlight
  if (identical(highlight, "default"))
    highlight <- "pygments"
  
  # get the tufte handlout template
  template <-  rmarkdown_system_file(
    "rmarkdown", "templates", "tufte_handout", "resources", "tufte-handout.tex"
  )
  
  # call the base pdf_document format with the appropriate options
  format <- rmarkdown::pdf_document(
    fig_width = fig_width, fig_height = fig_height, fig_crop = fig_crop,
    dev = dev, highlight = highlight, template = template, ...
  )

  knitr::knit_engines$set(marginfigure = function(options) {
    options$type <- 'marginfigure'
    eng_block <- knitr::knit_engines$get('block')
    eng_block(options)
  })

  # create knitr options (ensure opts and hooks are non-null)
  knitr_options <- knitr_options_pdf(fig_width, fig_height, fig_crop, dev)
  if (is.null(knitr_options$opts_knit))
    knitr_options$opts_knit <- list()
  if (is.null(knitr_options$knit_hooks))
    knitr_options$knit_hooks <- list()
  
  # set options
  knitr_options$opts_chunk$tidy <- TRUE
  knitr_options$opts_knit$width <- 45
  
  # set hooks for special plot output
  knitr_options$knit_hooks$plot <- function(x, options) {

    # determine figure type
    if (isTRUE(options$fig.margin)) 
      options$fig.env <- "marginfigure"
    else if (isTRUE(options$fig.fullwidth))
      options$fig.env <- "figure*"

    knitr::hook_plot_tex(x, options)
  }
  
  # override the knitr settings of the base format and return the format
  format$knitr <- knitr_options
  format
}

#' @details \code{tufte_html()} provides the HTML format based on the Tufte CSS:
#'   \url{https://edwardtufte.github.io/tufte-css/}.
#' @rdname tufte_handout
#' @export
tufte_html <- function(...) {
  tufte_css <- rmarkdown_system_file(
    'rmarkdown', 'templates', 'tufte_html', 'resources', 'tufte.css'
  )
  format <- html_document(theme = NULL, ..., extra_dependencies = tufte_html_dependency())
  format$post_processor <- function(metadata, input, output, clean, verbose) {
    x <- read_lines_utf8(output, 'UTF-8')
    footnotes <- parse_footnotes(x)
    notes <- footnotes$items
    # replace footnotes with sidenotes
    for (i in seq_along(notes)) {
      num <- sprintf(
        '<a href="#fn%d" class="footnoteRef" id="fnref%d"><sup>%d</sup></a>',
        i, i, i
      )
      con <- sprintf(paste0(
        '<label for="tufte-sn-%d" class="margin-toggle sidenote-number">%d</label>',
        '<input type="checkbox" id="tufte-sn-%d" class="margin-toggle">',
        '<span class="sidenote"><span class="sidenote-number">%d</span> %s</span>'
      ), i, i, i, i, notes[i])
      x <- sub(num, con, x, fixed = TRUE)
    }
    # remove footnotes at the bottom
    if (length(footnotes$range)) x <- x[-footnotes$range]
    # replace citations with margin notes
    x <- margin_references(x)
    # place figure captions in margin notes
    x[x == '<p class="caption">'] <- marginnote_html('\n<p class="caption marginnote">')
    # add an incremental number to the id of <label> and <input> for margin notes
    r <- '(<label|<input type="checkbox") (id|for)(="tufte-mn)-(" )'
    mn <- grep(r, x)
    for (i in seq_along(mn)) {
      x[mn[i]] <- gsub(r, paste0('\\1 \\2\\3-', i, '\\4'), x[mn[i]])
    }
    writeLines(enc2utf8(x), output, useBytes = TRUE)
    output
  }
  if (is.null(format$knitr$knit_hooks)) format$knitr$knit_hooks <- list()
  format$knitr$knit_hooks$plot <- function(x, options) {
    # make sure the plot hook always generates HTML code instead of ![]()
    if (is.null(options$out.extra)) options$out.extra <- ''
    fig_margin <- isTRUE(options$fig.margin)
    fig_fullwd <- isTRUE(options$fig.fullwidth)
    # for normal figures, place captions at the top of images
    if (!fig_margin && !fig_fullwd && is.null(options$fig.topcaption))
      options$fig.topcaption <- TRUE
    res <- knitr::hook_plot_md(x, options)
    if (fig_margin) {
      res <- gsub_fixed('<p class="caption">', '<!--\n<p class="caption ">-->', res)
      res <- gsub_fixed('</p>', '<!--</p>-->', res)
      res <- gsub_fixed('</div>', '<!--</div>--></span></p>', res)
      res <- gsub_fixed(
        '<div class="figure">', paste0(
          '<p>', marginnote_html(), '<span class="marginnote">',
          '<!--\n<div class="figure">-->'
        ), res
      )
    } else if (fig_fullwd) {
      res <- gsub_fixed('<div class="figure">', '<div class="figure fullwidth">', res)
      res <- gsub_fixed('<p class="caption">', '<p class="caption fullwidth">', res)
    }
    res
  }

  knitr::knit_engines$set(marginfigure = function(options) {
    options$type <- 'marginnote'
    options$html.tag <- 'span'
    options$html.before <- marginnote_html()
    eng_block <- knitr::knit_engines$get('block')
    eng_block(options)
  })

  format
}

tufte_html_dependency <- function() {
  list(htmlDependency(
    'tufte-css', '2015.12.29',
    src = rmarkdown_system_file('rmarkdown', 'templates', 'tufte_html', 'resources'),
    stylesheet = 'tufte.css'
  ))
}

# we assume one footnote only contains one paragraph here, although it is
# possible to write multiple paragraphs in a footnote with Pandoc's Markdown
parse_footnotes <- function(x) {
  i <- which(x == '<div class="footnotes">')
  if (length(i) == 0) return(list(items = character(), range = integer()))
  j <- which(x == '</div>')
  j <- min(j[j > i])
  n <- length(x)
  r <- '<li id="fn([0-9]+)"><p>(.+)<a href="#fnref\\1">.</a></p></li>'
  list(
    items = gsub(r, '\\2', grep(r, x[i:n], value = TRUE)),
    range = i:j
  )
}

margin_references <- function(x) {
  i <- which(x == '<div id="refs" class="references">')
  if (length(i) != 1) return(x)
  r <- '^<div id="(ref-[^"]+?)">$'
  k <- grep(r, x)
  k <- k[k > i]
  n <- length(k)
  if (n == 0) return(x)
  ids <- gsub(r, '\\1', x[k])
  ids <- sprintf('<a href="#%s">(.+?)</a>', ids)
  ref <- gsub('^<p>|</p>$', '', x[k + 1])
  ref <- marginnote_html(paste0('\\1<span class="marginnote">', ref, '</span>'))
  for (j in seq_len(n)) {
    x <- gsub(ids[j], ref[j], x)
  }
  x[-(i:(max(k) + 3))]  # remove references at the bottom
}

#' @details \code{newthought()} can be used in inline R expressions in R
#'   Markdown (e.g. \samp{`r newthought(Some text)`}), and it works for both
#'   HTML (\samp{<span class="newthought">text</span>}) and PDF
#'   (\samp{\\newthought{text}}) output.
#' @param text A character string to be presented as a \dQuote{new thought}
#'   (using small caps), or a margin note
#' @rdname tufte_handout
#' @export
newthought <- function(text) {
  if (is_html_output()) {
    sprintf('<span class="newthought">%s</span>', text)
  } else if (is_latex_output()) {
    sprintf('\\newthought{%s}', text)
  } else {
    sprintf('<span style="font-variant:small-caps;">%s</span>', text)
  }
}

#' @details \code{marginnote()} can be used in inline R expressions to write a
#'   margin note (like a sidenote but not numbered)
#' @param icon A character string to indicate there is a hidden margin note when
#'   the page width is too narrow (by default it is a circled plus sign)
#' @rdname tufte_handout
#' @export
marginnote <- function(text, icon = '&#8853;') {
  if (is_html_output()) {
    marginnote_html(sprintf('<span class="marginnote">%s</span>', text), icon)
  } else if (is_latex_output()) {
    sprintf('\\marginnote{%s}', text)
  } else {
    warning('marginnote() only works for HTML and LaTeX output', call. = FALSE)
    text
  }
}

marginnote_html <- function(text = '', icon = '&#8853;') {
  sprintf(paste0(
    '<label for="tufte-mn-" class="margin-toggle">%s</label>',
    '<input type="checkbox" id="tufte-mn-" class="margin-toggle">%s'
  ), icon, text)
}
