#' Convert to a PDF/LaTeX document
#'
#' Formats for converting from R Markdown to a PDF or LaTeX document.
#'
#' See the \href{https://bookdown.org/yihui/rmarkdown/pdf-document.html}{online
#' documentation} for additional details on using the \code{pdf_document}
#' format.
#'
#' Creating PDF output from R Markdown requires that LaTeX be installed.
#'
#' R Markdown documents can have optional metadata that is used to generate a
#' document header that includes the title, author, and date. For more details
#' see the documentation on R Markdown \link[=rmd_metadata]{metadata}.
#'
#' R Markdown documents also support citations. You can find more information on
#' the markdown syntax for citations in the
#' \href{https://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html}{Bibliographies
#' and Citations} article in the online documentation.
#'
#' Many aspects of the LaTeX template used to create PDF documents can be
#' customized using metadata. For example:
#'
#' \tabular{l}{
#' \code{---} \cr
#' \code{title: "Crop Analysis Q3 2013"} \cr
#' \code{fontsize: 11pt} \cr
#' \code{geometry: margin=1in} \cr
#' \code{---}
#' }
#'
#' Available metadata variables include:
#'
#' \describe{
#'    \item{\code{lang}}{Document language code (e.g. "es", "fr", "pt-BR")}
#'    \item{\code{fontsize}}{Font size (e.g. 10pt, 11pt, 12pt)}
#'    \item{\code{documentclass}}{LaTeX document class (e.g. article)}
#'    \item{\code{classoption}}{Option for \code{documentclass} (e.g. oneside); may be repeated}
#'    \item{\code{geometry}}{Options for geometry class (e.g. margin=1in); may be repeated}
#'    \item{\code{mainfont, sansfont, monofont, mathfont}}{Document fonts (works only with xelatex and lualatex, see the \code{latex_engine} option)}
#'    \item{\code{linkcolor, urlcolor, citecolor}}{Color for internal, external, and citation links (red, green, magenta, cyan, blue, black)}
#'    \item{\code{linestretch}}{Options for line spacing (e.g. 1, 1.5, 3)}
#' }
#' @inheritParams html_document
#' @param fig_crop Whether to crop PDF figures with the command
#'   \command{pdfcrop}. This requires the tools \command{pdfcrop} and
#'   \command{ghostscript} to be installed. By default, \code{fig_crop = TRUE}
#'   if these two tools are available.
#' @param dev Graphics device to use for figure output (defaults to pdf)
#' @param highlight Syntax highlighting style. Supported styles include
#'   "default", "tango", "pygments", "kate", "monochrome", "espresso",
#'   "zenburn", and "haddock". Pass \code{NULL} to prevent syntax highlighting.
#' @param keep_tex Keep the intermediate tex file used in the conversion to PDF
#' @param latex_engine LaTeX engine for producing PDF output. Options are
#'   "pdflatex", "lualatex", "xelatex" and "tectonic".
#' @param citation_package The LaTeX package to process citations, \code{natbib}
#'   or \code{biblatex}. Use \code{default} if neither package is to be used,
#'   which means citations will be processed via the command
#'   \command{pandoc-citeproc}.
#' @param template Pandoc template to use for rendering. Pass "default" to use
#'   the rmarkdown package default template; pass \code{NULL} to use pandoc's
#'   built-in template; pass a path to use a custom template that you've
#'   created.  See the documentation on
#'   \href{https://pandoc.org/MANUAL.html}{pandoc online documentation} for
#'   details on creating custom templates.
#' @param output_extensions Pandoc extensions to be added or removed from the
#'   output format, e.g., \code{"-smart"} means the output format will be
#'   \code{latex-smart}.
#' @param extra_dependencies A LaTeX dependency \code{latex_dependency()}, a
#'   list of LaTeX dependencies, a character vector of LaTeX package names (e.g.
#'   \code{c("framed", "hyperref")}), or a named list of LaTeX package options
#'   with the names being package names (e.g. \code{list(hyperef =
#'   c("unicode=true", "breaklinks=true"), lmodern = NULL)}). It can be used to
#'   add custom LaTeX packages to the .tex header.
#' @return R Markdown output format to pass to \code{\link{render}}
#' @examples
#' \dontrun{
#' library(rmarkdown)
#'
#' # simple invocation
#' render("input.Rmd", pdf_document())
#'
#' # specify an option for latex engine
#' render("input.Rmd", pdf_document(latex_engine = "lualatex"))
#'
#' # add a table of contents and pass an option to pandoc
#' render("input.Rmd", pdf_document(toc = TRUE, "--listings"))
#' }
#' @export
pdf_document <- function(toc = FALSE,
                         toc_depth = 2,
                         number_sections = FALSE,
                         fig_width = 6.5,
                         fig_height = 4.5,
                         fig_crop = 'auto',
                         fig_caption = TRUE,
                         dev = 'pdf',
                         df_print = "default",
                         highlight = "default",
                         template = "default",
                         keep_tex = FALSE,
                         keep_md = FALSE,
                         latex_engine = "pdflatex",
                         citation_package = c("default", "natbib", "biblatex"),
                         includes = NULL,
                         md_extensions = NULL,
                         output_extensions = NULL,
                         pandoc_args = NULL,
                         extra_dependencies = NULL) {

  # base pandoc options for all PDF output
  args <- c("--self-contained")

  # table of contents
  args <- c(args, pandoc_toc_args(toc, toc_depth))

  append_in_header <- function(text, file = as_tmpfile(text)) {
    includes_to_pandoc_args(includes(in_header = file))
  }

  # template path and assets
  if (!is.null(template) && !identical(template, "default")) {
    args <- c(args, "--template", pandoc_path_arg(template))
  }

  # numbered sections
  if (number_sections)
    args <- c(args, "--number-sections")

  # highlighting
  if (!is.null(highlight))
    highlight <- match.arg(highlight, highlighters())
  args <- c(args, pandoc_highlight_args(highlight))

  # latex engine
  latex_engine <- match.arg(latex_engine, c("pdflatex", "lualatex", "xelatex", "tectonic"))
  args <- c(args, pandoc_latex_engine_args(latex_engine))

  # citation package
  args <- c(args, citation_package_arg(citation_package))

  # content includes
  args <- c(args, includes_to_pandoc_args(includes))

  # make sure the graphics package is always loaded
  if (identical(template, "default")) args <- c(args, "--variable", "graphics")

  # args args
  args <- c(args, pandoc_args)

  saved_files_dir <- NULL

  # Use filter to set pdf geometry defaults (while making sure we don't override
  # any geometry settings already specified by the user)
  pdf_pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir,
                                output_dir) {

    # make sure --include-in-header from command line will not completely
    # override header-includes in metadata but give the latter lower precedence:
    # https://github.com/rstudio/rmarkdown/issues/1359
    args <- append_in_header(process_header_includes(metadata))

    # use a geometry filter when we are using the "default" template
    if (identical(template, "default")) {
      # set the margin to 1 inch if no geometry options or document class specified
      if (default_geometry(names(metadata), pandoc_args))
        args <- c(args, "--variable", "geometry:margin=1in")
      # support subtitle for Pandoc < 2.6
      if (("subtitle" %in% names(metadata)) && !pandoc_available("2.6")) args <- c(
        args, append_in_header(file = pkg_file("rmd/latex/subtitle.tex"))
      )
    }

    if (length(extra_dependencies) || has_latex_dependencies(knit_meta)) {
      extra_dependencies <- latex_dependencies(extra_dependencies)
      all_dependencies <- append(extra_dependencies, flatten_latex_dependencies(knit_meta))
      args <- c(args, append_in_header(latex_dependencies_as_string(all_dependencies)))
    }
    args
  }


  pre_processor <- function(metadata, input_file, runtime, knit_meta,
                                files_dir, output_dir) {
    # save files dir (for generating intermediates)
    saved_files_dir <<- files_dir

    pdf_pre_processor(metadata, input_file, runtime, knit_meta, files_dir,
                      output_dir)
  }

  intermediates_generator <- function(...) {
    general_intermediates_generator(saved_files_dir, ...)
  }

  # return format
  output_format(
    knitr = knitr_options_pdf(fig_width, fig_height, fig_crop, dev),
    pandoc = pandoc_options(
      to = paste(c("latex", output_extensions), collapse = ""),
      from = from_rmarkdown(fig_caption, md_extensions),
      args = args,
      latex_engine = latex_engine,
      keep_tex = keep_tex,
      lua_filters = pkg_file_lua(c("pagebreak.lua", "latex-div.lua"))
    ),
    clean_supporting = !keep_tex,
    keep_md = keep_md,
    df_print = df_print,
    pre_processor = pre_processor,
    intermediates_generator = intermediates_generator
  )
}

general_intermediates_generator <- function(
  saved_files_dir, original_input, intermediates_dir
) {

  # copy all intermediates (pandoc will need to bundle them in the PDF)
  intermediates <- copy_render_intermediates(original_input, intermediates_dir, FALSE)

  # we need figures from the supporting files dir to be available during
  # render as well; if we have a files directory, copy its contents
  if (!is.null(saved_files_dir) && dir_exists(saved_files_dir)) {
    file.copy(saved_files_dir, intermediates_dir, recursive = TRUE)
    intermediates <- c(intermediates, list.files(
      path = file.path(intermediates_dir, basename(saved_files_dir)),
      all.files = TRUE, recursive = TRUE, full.names = TRUE))
  }

  intermediates
}

patch_tex_output <- function(file) {
  x <- read_utf8(file)
  if (length(i <- which(x == '\\begin{document}')) == 0) return()
  if (length(i <- grep('^\\\\date\\{', head(x, i[1]))) == 0) return()

  i <- i[1]
  # add \author{} if missing: https://github.com/jgm/pandoc/pull/5961
  if (length(grep('^\\\\author\\{', head(x, i))) == 0) {
    x <- append(x, '\\author{}', i - 1)
    i <- i + 1
  }
  # reduce the vertical spacing in \date{} if no author is given
  if (any(head(x, i) == '\\author{}')) {
    x[i] <- paste0('\\date{\\vspace{-2.5em}', sub('^\\\\date\\{', '', x[i]))
  }
  write_utf8(x, file)
}

# patch output from Pandoc < 2.8: https://github.com/jgm/pandoc/issues/5801
fix_horiz_rule <- function(file) {
  if (pandoc_available('2.8')) return()
  x <- read_utf8(file)
  i <- x == '\\begin{center}\\rule{0.5\\linewidth}{\\linethickness}\\end{center}'
  if (any(i)) {
    x[i] <- '\\begin{center}\\rule{0.5\\linewidth}{0.5pt}\\end{center}'
    write_utf8(x, file)
  }
}

process_header_includes <- function(x) {
  x <- unlist(x[["header-includes"]])
  gsub('(^|\n)\\s*```\\{=latex\\}\n(.+?\n)```\\s*(\n|$)', '\\1\\2\\3', x)
}

citation_package_arg <- function(value) {
  value <- value[1]
  if (value == "none") {
    warning("citation_package = 'none' was deprecated; please use 'default' instead.")
    value <- "default"
  }
  value <- match.arg(value, c("default", "natbib", "biblatex"))
  if (value != "default") paste0("--", value)
}

default_geometry <- function(meta_names, pandoc_args = NULL) {
  !any(c('geometry', 'documentclass') %in% meta_names) &&
    length(grep('^(--(variable|metadata)=)?documentclass:', pandoc_args)) == 0
}

#' @param ... Arguments passed to \code{pdf_document()}.
#' @rdname pdf_document
#' @export
latex_document <- function(...) {
  merge_lists(pdf_document(..., keep_tex = TRUE), list(pandoc = list(ext = ".tex")))
}

#' @rdname pdf_document
#' @export
latex_fragment <- function(...) {
  latex_document(..., template = pkg_file("rmd/fragment/default.tex"))
}
