#' Convert a Jupyter/IPython notebook to an R Markdown document
#'
#' Read a Jupyter/IPython notebook file (\file{.ipynb}) via
#' \code{jsonlite::fromJSON()}, convert its code cells to R Markdown code
#' chunks, preserve Markdown cells, and write out the results to an Rmd file.
#'
#' This simple converter may have some rough edges, depending on how many
#' IPython-specific features are used in a notebook. For example, line magics
#' are not automatically converted (warnings will be issued if line magics are
#' detected), but you may consider using or writing R functions to replace them
#' in R Markdown (e.g., the \command{\%load} magic may be replaced by
#' \code{reticulate::source_python()}). Cell magics will be converted to code
#' chunks with the (\pkg{knitr}) language engine names being the magic names.
#' For example, the cell magic \command{\%\%js} is converted to \verb{```{js}}
#' in R Markdown. This does not always work because not all IPython cell magics
#' have their counterparts in \pkg{knitr}'s language engines, but common cell
#' magics like \command{\%\%bash}, \command{\%\%sh}, \command{\%\%js},
#' \command{\%\%perl}, \command{\%\%python}, and \command{\%\%ruby} should work.
#' @param input Path to the input \file{.ipynb} file.
#' @param output The output file path.
#' @return The output file path (invisibly).
#' @keywords internal
#' @examples
#' # this is not a real ipynb file, but illustrates what convert_ipynb() does
#' nb_data <- list(
#'   cells = list(
#'     list(cell_type = 'markdown', source = 'Hi **Markdown**!'),
#'     list(cell_type = 'code', source = 'print("Hi R Markdown!")')
#'   ),
#'   metadata = list(
#'     kernelspec = list(language = 'python')
#'   )
#' )
#' nb_file = tempfile(fileext = '.ipynb')
#' jsonlite::write_json(nb_data, nb_file, auto_unbox = TRUE, pretty = TRUE)
#' xfun::file_string(nb_file)  # show file content
#'
#' # convert to R Markdown
#' nb_rmd = rmarkdown:::convert_ipynb(nb_file)
#' xfun::file_string(nb_rmd)
convert_ipynb <- function(input, output = xfun::with_ext(input, 'Rmd')) {
  json <- jsonlite::fromJSON(input, simplifyDataFrame = FALSE)
  spec <- json$metadata$kernelspec
  lang <- spec$language  # global language
  if (is.null(lang) && identical(tolower(spec$name), 'ir')) lang <- 'r'
  if (is.null(lang)) lang <- 'python'  # fall back to python
  res <- character()
  for (cell in json$cells) {
    if (length(src <- unlist(cell$source)) == 0) next  # empty cell
    src <- gsub('\n$', '', src)
    src <- switch(
      cell$cell_type,
      code = cell_chunk(src, lang, cell$metadata),
      raw  = cell_raw(src, cell$metadata$format),
      src
    )
    res <- c(res, src, '')
  }
  res <- c('---', ipynb_yaml(json$metadata, input), '---\n', res)
  xfun::write_utf8(res, output)
  invisible(output)
}

# convert an ipynb cell to an Rmd chunk
cell_chunk <- function(x, lang, meta = list()) {
  if (length(x) == 0) return()
  # warn against line magics
  if (length(i <- grep(r_line_magics, x)) > 0) warning(
    'Detected the following probable line magics. They do not work in R Markdown.\n\n',
    paste(' ', x[i], collapse = '\n'), call. = FALSE
  )
  # replace cell magics with knitr language engines
  if (grepl(r <- '^%%([[:alnum:]]+)\\s*$', x[1])) {
    lang <- gsub(r, '\\1', x[1]); x <- x[-1]
  }
  if (lang == 'markdown') return(x)
  opts <- c('', meta$name)  # cell name (if defined) to chunk label
  meta <- meta$jupyter  # convert some jupyter cell metadata to chunk options
  opts <- c(
    opts, if (isTRUE(meta$source_hidden)) 'echo=FALSE',
    if (isTRUE(meta$outputs_hidden)) 'results="hide"'
  )
  c(sprintf('```{%s%s}', adjust_lang(lang), paste(opts, collapse = ', ')), x, '```')
}

line_magics <- c(
  'alias', 'alias_magic', 'autoawait', 'autocall', 'automagic',
  'autosave', 'bookmark', 'cat', 'cd', 'clear', 'colors', 'conda',
  'config', 'connect_info', 'cp', 'debug', 'dhist', 'dirs', 'doctest_mode',
  'ed', 'edit', 'env', 'gui', 'hist', 'history', 'killbgscripts',
  'ldir', 'less', 'lf', 'lk', 'll', 'load', 'load_ext', 'loadpy',
  'logoff', 'logon', 'logstart', 'logstate', 'logstop', 'ls', 'lsmagic',
  'lx', 'macro', 'magic', 'man', 'matplotlib', 'mkdir', 'more',
  'mv', 'notebook', 'page', 'pastebin', 'pdb', 'pdef', 'pdoc',
  'pfile', 'pinfo', 'pinfo2', 'pip', 'popd', 'pprint', 'precision',
  'prun', 'psearch', 'psource', 'pushd', 'pwd', 'pycat', 'pylab',
  'qtconsole', 'quickref', 'recall', 'rehashx', 'reload_ext', 'rep',
  'rerun', 'reset', 'reset_selective', 'rm', 'rmdir', 'run', 'save',
  'sc', 'set_env', 'store', 'sx', 'system', 'tb', 'time', 'timeit',
  'unalias', 'unload_ext', 'who', 'who_ls', 'whos', 'xdel', 'xmode'
)

r_line_magics <- paste0('^%?(', paste(line_magics, collapse = '|'), ')($|\\s)')

# convert raw text/html and text/latex cells to raw ```{=format}` Markdown blocks
cell_raw <- function(x, fmt) {
  if (length(fmt) != 1) return()
  fmt <- switch(fmt, 'text/html' = 'html', 'text/latex' = 'latex')
  if (length(fmt) == 0) return()
  c(sprintf('```{=%s}', fmt), x, '```')
}

# adjust some cell magic names to knitr's engine names
adjust_lang <- function(x) {
  if (x == 'R') return('r')
  if (x == 'javascript') return('js')
  # use raw HTML/LaTeX blocks for Pandoc's Markdown
  if (tolower(x) %in% c('html', 'latex')) return(paste0('=', tolower(x)))
  x
}

# convert .ipynb metadata to YAML frontmatter in .Rmd
ipynb_yaml <- function(meta, input) {
  # default title and output format
  res <- list(
    title = paste0('An R Markdown document converted from "', input, '"'),
    output = 'html_document'
  )
  # currently only the `authors` field is supported
  authors <- unlist(lapply(meta$authors, `[[`, 'name'))
  if (length(authors) > 0) res[['author']] <- paste(authors, collapse = ', ')
  gsub('\n+$', '', yaml::as.yaml(res))
}
