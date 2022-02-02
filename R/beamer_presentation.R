#' Convert to a Beamer presentation
#'
#' Format for converting from R Markdown to a Beamer presentation.
#'
#' See the [online documentation](https://bookdown.org/yihui/rmarkdown/beamer-presentation.html)
#' for additional details on using the `beamer_presentation` format.
#'
#' Creating Beamer output from R Markdown requires that LaTeX be installed.
#'
#' R Markdown documents can have optional metadata that is used to generate a
#' document header that includes the title, author, and date. For more details
#' see the documentation on R Markdown [metadata][rmd_metadata].
#'
#' R Markdown documents also support citations. You can find more information on
#' the markdown syntax for citations in the
#' [Bibliographies and Citations](https://pandoc.org/MANUAL.html#citations)
#' article in the online documentation.
#' @inheritParams pdf_document
#' @inheritParams html_document
#' @param toc `TRUE` to include a table of contents in the output (only
#'   level 1 headers will be included in the table of contents).
#' @param slide_level The heading level which defines individual slides. By
#'   default this is the highest header level in the hierarchy that is followed
#'   immediately by content, and not another header, somewhere in the document.
#'   This default can be overridden by specifying an explicit
#'   `slide_level`.
#' @param incremental `TRUE` to render slide bullets incrementally. Note
#'   that if you want to reverse the default incremental behavior for an
#'   individual bullet you can precede it with `>`. For example:
#'   *`> - Bullet Text`*. See more in
#'   [Pandoc's Manual](https://pandoc.org/MANUAL.html#incremental-lists)
#' @param theme Beamer theme (e.g. "AnnArbor").
#' @param colortheme Beamer color theme (e.g. "dolphin").
#' @param fonttheme Beamer font theme (e.g. "structurebold").
#' @param self_contained Whether to generate a full LaTeX document (`TRUE`)
#'   or just the body of a LaTeX document (`FALSE`). Note the LaTeX
#'   document is an intermediate file unless `keep_tex = TRUE`.
#' @return R Markdown output format to pass to [render()]
#' @examples
#' \dontrun{
#'
#' library(rmarkdown)
#'
#' # simple invocation
#' render("pres.Rmd", beamer_presentation())
#'
#' # specify an option for incremental rendering
#' render("pres.Rmd", beamer_presentation(incremental = TRUE))
#' }
#' @md
#' @export
beamer_presentation <- function(toc = FALSE,
                                slide_level = NULL,
                                number_sections = FALSE,
                                incremental = FALSE,
                                fig_width = 10,
                                fig_height = 7,
                                fig_crop = 'auto',
                                fig_caption = TRUE,
                                dev = 'pdf',
                                df_print = "default",
                                theme = "default",
                                colortheme = "default",
                                fonttheme = "default",
                                highlight = "default",
                                template = "default",
                                keep_tex = FALSE,
                                keep_md = FALSE,
                                latex_engine = "pdflatex",
                                citation_package = c("default", "natbib", "biblatex"),
                                self_contained = TRUE,
                                includes = NULL,
                                md_extensions = NULL,
                                pandoc_args = NULL) {

  # base pandoc options for all beamer output
  args <- c()

  # template path and assets
  if (!is.null(template)) {
    if (identical(template, "default")) template <- patch_beamer_template()
    if (!is.null(template))
      args <- c(args, "--template", pandoc_path_arg(template))
  }

  # table of contents
  if (toc)
    args <- c(args, "--table-of-contents")

  # slide level
  if (!is.null(slide_level))
    args <- c(args, "--slide-level", as.character(slide_level))

  if (number_sections)
    args <- c(args, "--number-sections")

  # incremental
  if (incremental)
    args <- c(args, "--incremental")

  # themes
  if (!identical(theme, "default"))
    args <- c(args, pandoc_variable_arg("theme", theme))
  if (!identical(colortheme, "default"))
    args <- c(args, pandoc_variable_arg("colortheme", colortheme))
  if (!identical(fonttheme, "default"))
    args <- c(args, pandoc_variable_arg("fonttheme", fonttheme))

  # highlighting
  if (!is.null(highlight)) highlight <- resolve_highlight(highlight, highlighters())
  args <- c(args, pandoc_highlight_args(highlight))

  # latex engine
  latex_engine <- match.arg(latex_engine, c("pdflatex", "lualatex", "xelatex", "tectonic"))
  args <- c(args, pandoc_latex_engine_args(latex_engine))

  # citation package
  args <- c(args, citation_package_arg(citation_package))

  # generate a self-contained LaTeX document (including preamble)
  if (self_contained) args <- c(args, "--self-contained")

  # content includes
  args <- c(args, includes_to_pandoc_args(includes))

  # make sure the graphics package is always loaded
  if (identical(template, "default")) args <- c(args, "--variable", "graphics=yes")

  # custom args
  args <- c(args, pandoc_args)

  # initialize saved files dir
  saved_files_dir <- NULL

  pre_processor <- function(metadata, input_file, runtime, knit_meta,
                                files_dir, output_dir) {
    # save files dir (for generating intermediates)
    saved_files_dir <<- files_dir

    # make sure --include-in-header from command line will not completely
    # override header-includes in metadata but give the latter lower precedence:
    # https://github.com/rstudio/rmarkdown/issues/1359
    # Same in PDF for beamer
    # https://github.com/rstudio/rmarkdown/issues/2294
    args <- append_in_header(process_header_includes(metadata))

    # no-op other than caching dir location
    args
  }

  # generate intermediates (required to make resources available for publish)
  intermediates_generator <- function(...) {
    general_intermediates_generator(saved_files_dir, ...)
  }

  # return format
  output_format(
    knitr = knitr_options_pdf(fig_width, fig_height, fig_crop, dev),
    pandoc = pandoc_options(
      to = "beamer",
      from = from_rmarkdown(fig_caption, md_extensions),
      args = args,
      latex_engine = latex_engine,
      keep_tex = keep_tex,
      lua_filters = pkg_file_lua(c("pagebreak.lua", "latex-div.lua"))
    ),
    pre_processor = pre_processor,
    intermediates_generator = intermediates_generator,
    clean_supporting = !keep_tex,
    keep_md = keep_md,
    df_print = df_print
  )
}


patch_beamer_template_pagenumber <- function(template) {

  patch <- paste(
    "% Comment these out if you don't want a slide with just the",
    "% part/section/subsection/subsubsection title:", "\\AtBeginPart{",
    "  \\let\\insertpartnumber\\relax", "  \\let\\partname\\relax",
    "  \\frame{\\partpage}", "}", "\\AtBeginSection{",
    "  \\let\\insertsectionnumber\\relax", "  \\let\\sectionname\\relax",
    "  \\frame{\\sectionpage}", "}", "\\AtBeginSubsection{",
    "  \\let\\insertsubsectionnumber\\relax", "  \\let\\subsectionname\\relax",
    "  \\frame{\\subsectionpage}", "}",
    sep = "\n"
  )

  pasted <- one_string(template)
  patched <- sub(patch, "", pasted, fixed = TRUE)
  strsplit(patched, "\n", fixed = TRUE)[[1]]
}

patch_beamer_template_paragraph_spacing <- function(template) {

  patch <- c(
    "\\setlength{\\parindent}{0pt}",
    "\\setlength{\\parskip}{6pt plus 2pt minus 1pt}"
  )

  lines <- unlist(lapply(patch, function(line) {
    index <- grep(line, template, fixed = TRUE)
    if (length(index) == 1) index else -1
  }))

  # bail if we already have these lines in the document
  if (all(lines >= 0) && lines[[1]] == lines[[2]] - 1)
    return(template)

  # find patch location -- we insert before this line
  targetLine <- "\\setlength{\\emergencystretch}{3em}  % prevent overfull lines"
  targetIdx <- grep(targetLine, template, fixed = TRUE)
  if (!length(targetIdx))
    return(template)

  # insert patch
  c(
    utils::head(template, n = targetIdx - 1),
    patch,
    utils::tail(template, n = -(targetIdx - 1))
  )
}

patch_beamer_template <- function() {
  pandoc_available(error = TRUE)

  # invoke pandoc to read default template
  command <- paste(quoted(pandoc()), "-D beamer")
  template <- with_pandoc_safe_environment({
    tryCatch(
      system(command, intern = TRUE),
      error = function(e) NULL
    )
  })

  # make failure to read template non-fatal
  if (is.null(template))
    return(NULL)

  # trim whitespace
  template <- gsub("^\\s+|\\s+$", "", template, perl = TRUE)

  # apply patches (store original version of template so we can
  # compare after applying patches)
  original <- template
  version <- pandoc_version()

  if (version < "1.15.2")
    template <- patch_beamer_template_pagenumber(template)

  if (version > "1.15.2" && version < "1.17.3")
    template <- patch_beamer_template_paragraph_spacing(template)

  # if the template hasn't changed, return NULL (we don't need
  # to apply a custom template)
  if (identical(template, original))
    return(NULL)

  # write and return path to template
  as_tmpfile(enc2utf8(template))
}
