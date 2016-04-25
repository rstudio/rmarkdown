#' Convert to a PDF document
#'
#' Format for converting from R Markdown to a PDF document.
#'
#' @inheritParams html_document
#'
#' @param fig_crop \code{TRUE} to automatically apply the \code{pdfcrop} utility
#'   (if available) to pdf figures
#' @param dev Graphics device to use for figure output (defaults to pdf)
#' @param highlight Syntax highlighting style. Supported styles include
#'   "default", "tango", "pygments", "kate", "monochrome", "espresso",
#'   "zenburn", and "haddock". Pass \code{NULL} to prevent syntax highlighting.
#' @param keep_tex Keep the intermediate tex file used in the conversion to PDF
#' @param latex_engine LaTeX engine for producing PDF output. Options are
#'   "pdflatex", "lualatex", and "xelatex".
#' @param citation_package The LaTeX package to process citations, \code{natbib}
#'   or \code{biblatex}. Use \code{none} if neither package is to be used.
#' @param template Pandoc template to use for rendering. Pass "default" to use
#'   the rmarkdown package default template; pass \code{NULL} to use pandoc's
#'   built-in template; pass a path to use a custom template that you've
#'   created.  See the documentation on
#'   \href{http://pandoc.org/README.html}{pandoc online documentation}
#'   for details on creating custom templates.
#' @param extra_dependencies Add \code{latex_dependency()} dependencies. It can
#'   can be used to add custom LaTeX packages to the .tex header.
#'
#' @return R Markdown output format to pass to \code{\link{render}}
#'
#' @details
#'
#' See the \href{http://rmarkdown.rstudio.com/pdf_document_format.html}{online
#' documentation} for additional details on using the \code{pdf_document} format.
#'
#' Creating PDF output from R Markdown requires that LaTeX be installed.
#'
#' R Markdown documents can have optional metadata that is used to generate a
#' document header that includes the title, author, and date. For more details
#' see the documentation on R Markdown \link[=rmd_metadata]{metadata}.
#'
#' R Markdown documents also support citations. You can find more information on
#' the markdown syntax for citations in the
#' \href{http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html}{Bibliographies
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
#'    \item{\code{lang}}{Document language code}
#'    \item{\code{fontsize}}{Font size (e.g. 10pt, 11pt, 12pt)}
#'    \item{\code{documentclass}}{LaTeX document class (e.g. article)}
#'    \item{\code{classoption}}{Option for \code{documentclass} (e.g. oneside); may be repeated}
#'    \item{\code{geometry}}{Options for geometry class (e.g. margin=1in); may be repeated}
#'    \item{\code{mainfont, sansfont, monofont, mathfont}}{Document fonts (works only with xelatex and lualatex, see the \code{latex_engine} option)}
#'    \item{\code{linkcolor, urlcolor, citecolor}}{Color for internal, external, and citation links (red, green, magenta, cyan, blue, black)}
#'    \item{\code{linestretch}}{Options for line spacing (e.g. 1, 1.5, 3)}
#' }
#'
#' @examples
#' \dontrun{
#'
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
#'
#' @export
pdf_document <- function(toc = FALSE,
                         toc_depth = 2,
                         number_sections = FALSE,
                         fig_width = 6.5,
                         fig_height = 4.5,
                         fig_crop = TRUE,
                         fig_caption = TRUE,
                         dev = 'pdf',
                         highlight = "default",
                         template = "default",
                         keep_tex = FALSE,
                         latex_engine = "pdflatex",
                         citation_package = c("none", "natbib", "biblatex"),
                         includes = NULL,
                         md_extensions = NULL,
                         pandoc_args = NULL,
                         extra_dependencies = NULL) {

  # base pandoc options for all PDF output
  args <- c()

  # table of contents
  args <- c(args, pandoc_toc_args(toc, toc_depth))

  # template path and assets
  if (identical(template, "default")) {

    # choose the right template
    version <- pandoc_version()
    if (version >= "1.17.0.2")
      latex_template <- "default-1.17.0.2.tex"
    else if (version >= "1.15.2")
      latex_template <- "default-1.15.2.tex"
    else if (version >= "1.14")
      latex_template <- "default-1.14.tex"
    else
      latex_template <- "default.tex"

    # add to args
    args <- c(args, "--template",
              pandoc_path_arg(rmarkdown_system_file(paste0("rmd/latex/",
                                                           latex_template))))

  } else if (!is.null(template)) {
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
  latex_engine = match.arg(latex_engine, c("pdflatex", "lualatex", "xelatex"))
  args <- c(args, pandoc_latex_engine_args(latex_engine))

  # citation package
  citation_package <- match.arg(citation_package)
  if (citation_package != "none") args <- c(args, paste0("--", citation_package))

  # content includes
  args <- c(args, includes_to_pandoc_args(includes))

  # make sure the graphics package is always loaded
  if (identical(template, "default")) args <- c(args, "--variable", "graphics=yes")

  # args args
  args <- c(args, pandoc_args)

  saved_files_dir <- NULL

  # Use filter to set pdf geometry defaults (while making sure we don't override
  # any geometry settings already specified by the user)
  pdf_pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir,
                                output_dir) {

    args <- c()

    # set the margin to 1 inch if no other geometry options specified
    has_geometry <- function(text) {
      length(grep("^geometry:.*$", text)) > 0
    }
    if (!has_geometry(readLines(input_file, warn = FALSE)))
      args <- c(args, "--variable", "geometry:margin=1in")

    format_deps <- list()
    format_deps <- append(format_deps, extra_dependencies)

    if (has_latex_dependencies(knit_meta)) {
      all_dependencies <- if (is.null(format_deps)) list() else format_deps
      all_dependencies <- append(all_dependencies, flatten_latex_dependencies(knit_meta))
      filename <- tempfile()
      latex_dependencies_as_text_file(all_dependencies, filename)
      args <- c(args, includes_to_pandoc_args(includes(in_header = filename)))
    }
    args
  }


  pre_processor <- function(metadata, input_file, runtime, knit_meta,
                                files_dir, output_dir) {
    # save files dir (for generating intermediates)
    saved_files_dir <<- files_dir

    # use a geometry filter when we are using the "default" template
    if (identical(template, "default"))
      pdf_pre_processor(metadata, input_file, runtime, knit_meta, files_dir,
                        output_dir)
    else
      invisible(NULL)
  }

  intermediates_generator <- function(original_input, encoding,
                                      intermediates_dir) {
    # copy all intermediates (pandoc will need to bundle them in the PDF)
    intermediates <- copy_render_intermediates(original_input, encoding,
                                               intermediates_dir, FALSE)

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

  # return format
  output_format(
    knitr = knitr_options_pdf(fig_width, fig_height, fig_crop, dev),
    pandoc = pandoc_options(to = "latex",
                            from = from_rmarkdown(fig_caption, md_extensions),
                            args = args,
                            latex_engine = latex_engine,
                            keep_tex = keep_tex),
    clean_supporting = !keep_tex,
    pre_processor = pre_processor,
    intermediates_generator = intermediates_generator
  )
}
