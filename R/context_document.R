#' Convert to a ConTeXt document
#'
#' Format for converting from R Markdown to PDF using ConTeXt.
#'
#' ConTeXt needs to be installed, e.g., you can install it with
#' \code{tinytex::tlmgr_install("context")}.
#'
#' R Markdown documents can have optional metadata that is used to generate a
#' document header that includes the title, author, and date. For more details
#' see the documentation on R Markdown \link[=rmd_metadata]{metadata}.
#'
#' R Markdown documents also support citations. You can find more information on
#' the markdown syntax for citations in the
#' \href{https://pandoc.org/MANUAL.html#citations}{Bibliographies
#' and Citations} article in the online documentation.
#' @inheritParams pdf_document
#' @param context_path Path of the ConTeXt executable. If not provided, ConTeXt
#'   has to be available from the \code{PATH} environment variable.
#' @param context_args Command line arguments passed to ConTeXt.
#' @param ext Format of the output document (defaults to ".pdf").
#' @return R Markdown output format to pass to \code{\link{render}}.
#' @examples
#' \dontrun{
#' library(rmarkdown)
#'
#' # simple invocation
#' render("input.Rmd", context_document())
#' }
#' @export
context_document <- function(toc = FALSE,
                             toc_depth = 2,
                             number_sections = FALSE,
                             fig_width = 6.5,
                             fig_height = 4.5,
                             fig_crop = 'auto',
                             fig_caption = TRUE,
                             dev = "pdf",
                             df_print = "default",
                             template = NULL,
                             keep_tex = FALSE,
                             keep_md = FALSE,
                             citation_package = c("default", "natbib", "biblatex"),
                             includes = NULL,
                             md_extensions = NULL,
                             output_extensions = NULL,
                             pandoc_args = NULL,
                             context_path = NULL,
                             context_args = NULL,
                             ext = c(".pdf", ".tex")) {
  context_path <- normalize_path(context_path, must_work = TRUE)
  sys_context <- if (is.null(context_path)) find_program("context") else context_path
  ext <- match.arg(ext)
  if (identical(ext, ".pdf") && !nzchar(sys_context))
    stop2("Cannot find ConTeXt.\n",
         "Please, check that ConTeXt is installed.\n",
         "For more information, see the help page '?context_document'."
         )

  # base pandoc options for all ConTeXt output
  args <- c("--standalone")

  # Pass the path of ConTeXt
  if (!is.null(context_path)) {
    args <- c(args, pandoc_latex_engine_args(context_path))
  }

  # context command line arguments
  if (length(context_args))
    args <- c(args, paste("--pdf-engine-opt", context_args, sep = "="))

  # table of contents
  args <- c(args, pandoc_toc_args(toc, toc_depth))

  # numbered sections
  if (number_sections)
    args <- c(args, "--number-sections")

  # template
  if (!is.null(template))
    args <- c(args, "--template", pandoc_path_arg(template))

  # citation package
  args <- c(args, citation_package_arg(citation_package))

  # content includes
  args <- c(args, includes_to_pandoc_args(includes))

  # args args
  args <- c(args, pandoc_args)

  clean_supporting <- identical(ext, ".pdf") && !isTRUE(keep_tex)

  # post processor
  post_processor <- NULL
  # if keep_tex=TRUE, generate the ConTeXt file with Pandoc
  # and call ConTeXt using a post processor to generate the PDF file
  if (identical(ext, ".pdf") && isTRUE(keep_tex)) {
    ext <- ".tex" # Use Pandoc to generate the ConTeXt file
    post_processor <- function(metadata, input_file, output_file, clean, verbose) {
      context_args <- unique(c(
        context_args,
        # ConTeXt produces some auxiliary files:
        # direct PDF generation by Pandoc never produces these auxiliary
        # files because Pandoc runs ConTeXt in a temporary directory.
        # Replicate Pandoc's behavior using "--purgeall" option
        "--purgeall",
        # Pandoc runs ConteXt with "--batchmode" option. Do the same.
        "--batchmode"
      ))

      # ConTeXt is extremely verbose
      # Pandoc output these informations when run in its verbose mode
      # Replicate Pandoc's behavior
      is_pandoc_verbose <- !is.na(match("--verbose", pandoc_args))
      stdout <- if (is_pandoc_verbose) "" else FALSE
      system2(sys_context, c(output_file, context_args), stdout = stdout)
      xfun::with_ext(output_file, "pdf")
    }
  }

  # return format
  output_format(
    knitr = knitr_options_pdf(fig_width, fig_height, fig_crop, dev),
    pandoc = pandoc_options(
      to = paste(c("context", output_extensions), collapse = ""),
      from = from_rmarkdown(fig_caption, md_extensions),
      args = args,
      keep_tex = FALSE,
      ext = ext,
      lua_filters = pkg_file_lua("pagebreak.lua")
    ),
    clean_supporting = !isTRUE(keep_tex),
    keep_md = keep_md,
    df_print = df_print,
    intermediates_generator = general_intermediates_generator,
    post_processor = post_processor
  )
}
