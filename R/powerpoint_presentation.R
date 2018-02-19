#' Convert to a PowerPoint presentation
#'
#' Format for converting from R Markdown to a PowerPoint presentation. Pandoc
#' v2.0.5 or above is required.
#' @inheritParams pdf_document
#' @inheritParams html_document
#' @param reference_doc Path to a PowerPoint template.
#' @export
#' @return R Markdown output format to pass to \code{\link{render}}
powerpoint_presentation <- function(
  toc = FALSE, fig_width = 5, fig_height = 4, fig_caption = TRUE,
  df_print = 'default', smart = TRUE, keep_md = FALSE, md_extensions = NULL,
  reference_doc = 'default', pandoc_args = NULL
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
  if (toc) args <- c(args, '--toc')

  # ppt template
  if (!is.null(reference_doc) && !identical(reference_doc, 'default')) {
    args <- c(args, '--reference-doc', pandoc_path_arg(reference_doc))
  }

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
