#' Convert to GitHub Flavored Markdown
#'
#' Format for converting from R Markdown to GitHub Flavored Markdown.
#'
#' See the [online
#' documentation](https://rmarkdown.rstudio.com/github_document_format.html) for additional details on using the `github_document()`
#' format.
#' @inheritParams output_format
#' @inheritParams html_document
#' @inheritParams md_document
#' @param math_method `"default"` means that [native Github
#'   support](https://github.blog/changelog/2022-05-19-render-mathematical-expressions-in-markdown/)
#'    for math notations using Mathjax syntax will be used. Other possible value
#'   is `"webtex"` where equation will be rendered to an image in the resulting
#'   Markdown. See [html_document()] for option to change webtex URL. Set `math_method`to
#'   `NULL` to opt-out any math treatment.
#' @param hard_line_breaks `TRUE` to generate markdown that uses a simple
#'   newline to represent a line break (as opposed to two-spaces and a newline).
#' @param html_preview `TRUE` to also generate an HTML file for the purpose of
#'   locally previewing what the document will look like on GitHub.
#' @param keep_html `TRUE` to keep the preview HTML file in the working
#'   directory. Default is `FALSE`.
#'
#' @details # About Math support
#'
#' Default behavior is to keep any inline equation using `$` and any block
#' equation using `$$` in the resulting markdown as Github will process those
#' using Mathjax. **This feature is only available with Pandoc 2.10.1 and above**
#'
#' When using `"webtex"`, PNG images with a white background are used by default so
#' that it shows correctly on Github on both light and dark theme. You can
#' choose to only output SVG for better quality by changing the URL used:
#'
#' ```yaml
#' output:
#'   github_document:
#'     math_method:
#'       engine: webtex
#'       url: https://latex.codecogs.com/svg.image?
#' ```
#'
#' Background or fonts color cannot be changed for now and your equation may not be visible on dark theme.
#'
#' Using `"webtex"` will be the default with Pandoc 2.0.4 until Pandoc 2.10. Before 2.0.4, Github document output does not support math.
#'
#' @return R Markdown output format to pass to [render()]
#' @export
#' @md
github_document <- function(toc = FALSE,
                            toc_depth = 3,
                            number_sections = FALSE,
                            math_method = "default",
                            preserve_yaml = FALSE,
                            fig_width = 7,
                            fig_height = 5,
                            dev = 'png',
                            df_print = "default",
                            includes = NULL,
                            md_extensions = NULL,
                            hard_line_breaks = TRUE,
                            pandoc_args = NULL,
                            html_preview = TRUE,
                            keep_html = FALSE) {

  # add special markdown rendering template to ensure we include the title fields
  # and add an optional feature to number sections
  pandoc_args <- c(
    pandoc_args, "--template",
    pkg_file_arg("rmarkdown/templates/github_document/resources/default.md")
  )

  pandoc2 <- pandoc2.0()
  # use md_document as base
  if (pandoc2) {
    variant <-  "gfm"
  } else {
    variant <- "markdown_github"
  }
  if (!hard_line_breaks) variant <- paste0(variant, "-hard_line_breaks")

  # atx headers are the default in pandoc 2.11.2 and the flag has been deprecated
  # to be replace by `--markdown-headings=atx|setx`
  if (!pandoc_available("2.11.2")) pandoc_args <- c(
    "--atx-headers", pandoc_args
  )

  if ((toc || number_sections) && !isTRUE(grepl("gfm_auto_identifiers", md_extensions))) {
    md_extensions <- c(md_extensions, "+gfm_auto_identifiers")
  }

  # math support
  if (!is.null(math_method) && pandoc_available("2.0.4")) {
    math <- check_math_argument(math_method)
    preview_math <- NULL
    if (!math$engine %in% c("default", "webtex")) {
      stop("Markdown output format only support 'default' for native Github Math support or 'webtex' for using a Webtex online service to render math.")
    }
    if (math$engine == "default") {
      if (pandoc_available("2.10.1")) {
        # don't activate math in Pandoc and pass it as is
        # https://github.blog/changelog/2022-05-19-render-mathematical-expressions-in-markdown/
        math <- NULL
        # TODO: Check for version - should be the default in Pandoc 2.19+
        variant <- paste0(variant, "+tex_math_dollars")
        preview_math <- check_math_argument("mathjax")
      } else {
        # fallback to webtex
        math <- check_math_argument("webtex")
      }
    }
    if (!is.null(math) && math$engine == "webtex") {
      if (is.null(math$url)) {
        # default to png and white background for webtex
        math$url <- "https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;"
      }
      preview_math <- check_math_argument("webtex")
    }
    math <- add_math_support(math, NULL, NULL, NULL)
    preview_math <- add_math_support(preview_math, NULL, NULL, NULL)
    pandoc_args <- c(pandoc_args, math$args)
  }

  format <- md_document(
    variant = variant, toc = toc, toc_depth = toc_depth,
    number_sections = number_sections, preserve_yaml = preserve_yaml,
    fig_width = fig_width, fig_height = fig_height,
    dev = dev, df_print = df_print, includes = includes,
    md_extensions = md_extensions, pandoc_args = pandoc_args
  )

  # add a post processor for generating a preview if requested
  post <- format$post_processor

  format$post_processor <- function(metadata, input_file, output_file, clean, verbose) {

    if (is.function(post)) output_file <- post(metadata, input_file, output_file, clean, verbose)

    if (html_preview) {
      css <- pkg_file_arg(
        "rmarkdown/templates/github_document/resources/github.css")
      # provide a preview that looks like github
      args <- c(
        self_contained_args(), "--highlight-style", "pygments",
        "--template", pkg_file_arg(
          "rmarkdown/templates/github_document/resources/preview.html"),
        "--variable", paste0("github-markdown-css:", css),
        if (pandoc2) c("--metadata", "pagetitle=PREVIEW"),  # HTML5 requirement
        if (!is.null(preview_math)) preview_math$args
      )

      # run pandoc
      preview_file <- file_with_ext(output_file, "html")
      pandoc_convert(
        input = output_file, to = "html", from = variant, output = preview_file,
        options = args, verbose = verbose
      )

      # move the preview to the preview_dir if specified
      if (!keep_html) {
        preview_dir <- Sys.getenv("RMARKDOWN_PREVIEW_DIR", unset = NA)
        if (!is.na(preview_dir)) {
          relocated_preview_file <- tempfile("preview-", preview_dir, ".html")
          file.copy(preview_file, relocated_preview_file)
          file.remove(preview_file)
          preview_file <- relocated_preview_file
        }
      }

      if (verbose) message("\nPreview created: ", preview_file)

    }

    output_file
  }

  format  # return format
}

# explicit definition of gfm_format -- not currently used b/c it didn't
# yield different about that 'gfm' w/ pandoc 2.11. keeping in the source
# code for now in case we need to use it for another workaround.
gfm_format <- function() {
  paste0("markdown_strict",
   # commonmark
   "+raw_html",
   "+all_symbols_escapable",
   "+backtick_code_blocks",
   "+fenced_code_blocks",
   "+space_in_atx_header",
   "+intraword_underscores",
   "+lists_without_preceding_blankline",
   "+shortcut_reference_links",
   # gfm extensions
   "+auto_identifiers",
   "+autolink_bare_uris",
   "+emoji",
   "+gfm_auto_identifiers",
   "+pipe_tables",
   "+strikeout",
   "+task_lists"
  )
}
