#' Convert to a markdown document
#'
#' Format for converting from R Markdown to another variant of markdown (e.g.
#' strict markdown or github flavored markdown)
#'
#' See the \href{https://bookdown.org/yihui/rmarkdown/markdown-document.html}{online
#' documentation} for additional details on using the \code{md_document} format.
#'
#' R Markdown documents can have optional metadata that is used to generate a
#' document header that includes the title, author, and date. For more details
#' see the documentation on R Markdown \link[=rmd_metadata]{metadata}.
#' @inheritParams html_document
#' @param variant Markdown variant to produce (defaults to "markdown_strict").
#'   Other valid values are "commonmark", "gfm", "commonmark_x", "markdown_mmd",
#'   markdown_phpextra", "markdown_github", or even "markdown" (which produces
#'   pandoc markdown). You can also compose custom markdown variants, see the
#'   \href{https://pandoc.org/MANUAL.html}{pandoc online documentation} for
#'   details.
#' @param preserve_yaml Preserve YAML front matter in final document.
#' @param fig_retina Scaling to perform for retina displays. Defaults to
#'   \code{NULL} which performs no scaling. A setting of 2 will work for all
#'   widely used retina displays, but will also result in the output of
#'   \code{<img>} tags rather than markdown images due to the need to set the
#'   width of the image explicitly.
#' @param ext Extention of the output document (defaults to ".md").
#' @return R Markdown output format to pass to \code{\link{render}}
#' @examples
#' \dontrun{
#' library(rmarkdown)
#'
#' render("input.Rmd", md_document())
#'
#' render("input.Rmd", md_document(variant = "markdown_github"))
#' }
#' @export
md_document <- function(variant = "markdown_strict",
                        preserve_yaml = FALSE,
                        toc = FALSE,
                        toc_depth = 3,
                        number_sections = FALSE,
                        fig_width = 7,
                        fig_height = 5,
                        fig_retina = NULL,
                        dev = 'png',
                        df_print = "default",
                        includes = NULL,
                        md_extensions = NULL,
                        pandoc_args = NULL,
                        ext = ".md") {


  # base pandoc options for all markdown output
  args <- c(if (preserve_yaml) "--standalone")

  # table of contents
  args <- c(args, pandoc_toc_args(toc, toc_depth))

  # content includes
  args <- c(args, includes_to_pandoc_args(includes))

  # pandoc args
  args <- c(args, pandoc_args)

  # variants
  variant <- adapt_md_variant(variant, preserve_yaml)

  # add post_processor for yaml preservation if not supported by pandoc
  post_processor <- if (preserve_yaml && !grepl('yaml_metadata_block', variant, fixed = TRUE)) {
    function(metadata, input_file, output_file, clean, verbose) {
      input_lines <- read_utf8(input_file)
      partitioned <- partition_yaml_front_matter(input_lines)
      if (!is.null(partitioned$front_matter)) {
        output_lines <- c(partitioned$front_matter, "", read_utf8(output_file))
        write_utf8(output_lines, output_file)
      }
      output_file
    }
  }

  # return format
  output_format(
    knitr = knitr_options_html(fig_width, fig_height, fig_retina, FALSE, dev),
    pandoc = pandoc_options(
      to = variant,
      from = from_rmarkdown(extensions = md_extensions),
      args = args,
      ext = ext,
      lua_filters = if (number_sections) pkg_file_lua("number-sections.lua")
    ),
    clean_supporting = FALSE,
    df_print = df_print,
    post_processor = post_processor
  )
}

adapt_md_variant <- function(variant, preserve_yaml) {
  variant_base <- gsub("^([^+-]*).*", "\\1", variant)
  variant_extensions <- gsub(sprintf("^%s", variant_base), "", variant)

  set_extension <- function(format, ext, add = TRUE) {
    ext <- paste0(ifelse(add, "+", "-"), ext)
    if (grepl(ext, format, fixed = TRUE)) return(format)
    paste0(format, ext, collapse = "")
  }

  add_yaml_block_ext <- function(extensions, preserve_yaml) {
    set_extension(variant_extensions, "yaml_metadata_block", preserve_yaml)
  }

  # yaml_metadata_block extension
  variant_extensions <- switch(
    variant_base,
    gfm =,
    commonmark =,
    commonmark_x = {
      if (pandoc_available(2.13)) {
        add_yaml_block_ext(variant_extensions, preserve_yaml)
      } else {
        variant_extensions
      }
    },
    markdown =,
    markdown_phpextra =,
    markdown_github =,
    markdown_mmd =,
    markdown_strict = add_yaml_block_ext(variant_extensions, preserve_yaml),
    # do not modified for unknown (yet) md variant
    variant_extensions
  )

  paste0(variant_base, variant_extensions, collapse = "")
}
