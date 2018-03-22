#' Convert to a PowerPoint presentation
#'
#' Format for converting from R Markdown to a PowerPoint presentation. Pandoc
#' v2.0.5 or above is required.
#' @inheritParams pdf_document
#' @inheritParams html_document
#' @inheritParams beamer_presentation
#' @param reference_doc Path to a PowerPoint template.
#' @export
#' @return R Markdown output format to pass to \code{\link{render}}
powerpoint_presentation <- function(
  toc = FALSE, toc_depth = 2, fig_width = 5, fig_height = 4, fig_caption = TRUE,
  df_print = 'default', smart = TRUE, keep_md = FALSE, md_extensions = NULL,
  slide_level = NULL, reference_doc = 'default', pandoc_args = NULL
) {

  # PowerPoint has been supported since Pandoc 2.0.5
  pandoc_available('2.0.5', error = TRUE)

  # knitr options and hooks
  knitr <- knitr_options(opts_chunk = list(
    dev = 'png', dpi = 96, fig.width = fig_width, fig.height = fig_height
  ))

  # base pandoc options for all pptx output
  args <- c()

  # smart quotes, etc.
  if (smart) md_extensions <- c(md_extensions, '+smart')

  # table of contents
  args <- c(args, pandoc_toc_args(toc, toc_depth))

  # ppt template
  if (!is.null(reference_doc) && !identical(reference_doc, 'default')) {
    args <- c(args, '--reference-doc', pandoc_path_arg(reference_doc))
  }

  # slide level
  if (!is.null(slide_level))
    args <- c(args, '--slide-level', as.character(slide_level))

  # TODO: syntax highlighting

  # pandoc args
  args <- c(args, pandoc_args)

  # return output format
  output_format(
    knitr = knitr,
    pandoc = pandoc_options(
      to = 'pptx',
      from = from_rmarkdown(fig_caption, md_extensions),
      args = args
    ),
    keep_md = keep_md,
    df_print = df_print
  )
}
