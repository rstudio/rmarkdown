#' Convert to GitHub Flavored Markdown
#'
#' Format for converting from R Markdown to GitHub Flavored Markdown.
#'
#' @inheritParams html_document
#' @inheritParams md_document
#'
#' @param html_preview \code{TRUE} to also generate an HTML file for the purpose of
#'   locally previewing what the document will look like on GitHub.
#'
#' @return R Markdown output format to pass to \code{\link{render}}
#'
#' @export
github_document <- function(toc = FALSE,
                            toc_depth = 3,
                            fig_width = 7,
                            fig_height = 5,
                            dev = 'png',
                            includes = NULL,
                            md_extensions = NULL,
                            pandoc_args = NULL,
                            html_preview = TRUE) {

  # add special markdown rendering template to ensure we include the title fields
  pandoc_args <- c(pandoc_args, "--template",
                   pandoc_path_arg(rmarkdown_system_file(
                    "rmarkdown/templates/github_document/resources/default.md")))

  # use md_document as base
  format <- md_document(variant = "markdown_github",
                        toc = toc,
                        toc_depth = toc_depth,
                        fig_width = fig_width,
                        fig_height = fig_height,
                        dev = dev,
                        includes = includes,
                        md_extensions = md_extensions,
                        pandoc_args = pandoc_args)


  # add a post processor for generating a preview if requested
  if (html_preview) {
    format$post_processor <- function(metadata, input_file, output_file, clean, verbose) {

      # build pandoc args
      args <- c()

      # provide a preview that looks like github
      args <- c(args, "--standalone")
      args <- c(args, "--self-contained")
      args <- c(args, "--highlight-style", "pygments")
      args <- c(args, "--template",
                pandoc_path_arg(rmarkdown_system_file(
                  "rmarkdown/templates/github_document/resources/preview.html")))
      css <- pandoc_path_arg(rmarkdown_system_file(
        "rmarkdown/templates/github_document/resources/github.css"))
      args <- c(args, "--variable", paste("github-markdown-css:", css, sep=""))

      # no email obfuscation
      args <- c(args, "--email-obfuscation", "none")

      # run pandoc
      preview_file <- file_with_ext(output_file, "html")
      pandoc_convert(input = output_file,
                     to = "html",
                     from = "markdown_github",
                     output = preview_file,
                     options = args,
                     verbose = verbose)

      # move the preview to the preview_dir if specified
      preview_dir <- Sys.getenv("RMARKDOWN_PREVIEW_DIR", unset = NA)
      if (!is.na(preview_dir)) {
        relocated_preview_file <- tempfile(pattern = "preview-",
                                           tmpdir = preview_dir,
                                           fileext = ".html")
        file.copy(preview_file, relocated_preview_file)
        file.remove(preview_file)
        preview_file <- relocated_preview_file
      }

      if (verbose)
        message("\nPreview created: ", preview_file)

      output_file
    }
  }

  # return format
  format
}
