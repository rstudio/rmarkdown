#' Convert to GitHub Flavored Markdown
#' 
#' Format for converting from R Markdown to GitHub Flavored Markdown.
#' 
#' @inheritParams html_document
#'   
#' @return R Markdown output format to pass to \code{\link{render}}
#'   
#' @note An HTML file is also generated for the purpose of locally previewing what
#'   the document will look like on GitHub.
#'   
#' @details The markdown syntax supported in this document is GitHub Flavored
#'   Markdown (more details are here:
#'   \href{https://help.github.com/articles/github-flavored-markdown/}{https://help.github.com/articles/github-flavored-markdown/}).
#'   
#' @export
github_document <- function(fig_width = 7,
                            fig_height = 5) {
  
  # build pandoc args
  args <- c()
  
  # provide a preview that looks like github
  args <- c(args, "--standalone")
  args <- c(args, "--self-contained")
  args <- c(args, "--highlight-style", "pygments")
  args <- c(args, "--template",
            pandoc_path_arg(rmarkdown_system_file(
              "rmarkdown/templates/github_document/resources/default.html")))
  css <- pandoc_path_arg(rmarkdown_system_file(
              "rmarkdown/templates/github_document/resources/github.css"))
  args <- c(args, "--variable", paste("github-markdown-css:", css, sep=""))
  
  # no email obfuscation
  args <- c(args, "--email-obfuscation", "none")
  
  # pre-processor that returns an error if there is knit_meta
  pre_processor <- function (metadata, input_file, runtime, knit_meta,
                             files_dir, output_dir) {
    if (length(knit_meta) > 0) {
      stop("Rich HTML output (possibliy an html widget) found in ",
           "github_document. Documents published to GitHub can only ",
           "contain GitHub flavored markdown or very simple HTML.",
           call. = FALSE)
    } 
  }
  
  
  # return format
  output_format(
    knitr = knitr_options_html(fig_width = fig_width, 
                               fig_height = fig_height,
                               fig_retina = FALSE, 
                               keep_md = TRUE),
    pandoc = pandoc_options(to = "html",
                            from = "markdown_github+yaml_metadata_block",
                            args = args),
    keep_md = TRUE,
    clean_supporting = FALSE,
    pre_processor = pre_processor
  )
}
