#' Convert to an HTML vignette
#'
#' A HTML vignette is a lightweight alternative to [html_document()]
#' suitable for inclusion in packages to be released to CRAN. It reduces the
#' size of a basic vignette from 100k to around 10k.
#'
#' Compared to [html_document()], it:
#'
#' * never uses retina figures
#' * never uses a theme
#' * has a smaller default figure size
#' * uses a custom css stylesheet
#'
#' See the [online
#' documentation](https://bookdown.org/yihui/rmarkdown/r-package-vignette.html)
#' for additional details on using the `html_vignette()` format.
#' @inheritParams html_document
#' @param highlight,... Additional arguments passed to
#'   [html_document()]. Please note that `theme` and
#'   `fig_retina` are hard-coded. Setting any of those will yield an error.
#' @param css One or more css files to include.
#' @param readme Use this vignette as the package README.md file (i.e. render
#'   it as README.md to the package root). Note that if there are image files
#'   within your vignette you should be sure to add \file{README_files} to \file{.Rbuildignore}.
#' @param tabset Opt-in tabbed-sections feature inspired by [html_document()].
#'   See section "Tabbed Sections" for the detail.
#'   This feature also allows navigation to the tab from table of contents and URL.
#' @inheritSection html_document Tabbed Sections
#' @inheritSection html_document Code folding
#' @return R Markdown output format to pass to [render()]
#' @md
#' @export
html_vignette <- function(fig_width = 3,
                          fig_height = 3,
                          dev = 'png',
                          df_print = "default",
                          css = NULL,
                          highlight = "pygments",
                          keep_md = FALSE,
                          readme = FALSE,
                          self_contained = TRUE,
                          tabset = FALSE,
                          code_folding = c("none", "show", "hide"),
                          extra_dependencies = NULL,
                          pandoc_args = NULL,
                          ...) {

  lua_filters <- c()

  if (is.null(css)) {
    css <- system.file("rmarkdown", "templates", "html_vignette" ,"resources",
      "vignette.css", package = "rmarkdown")
  }

  if (tabset) {
    extra_dependencies <- append(extra_dependencies, list(html_dependency_tabset()))
  }

  code_folding <- match.arg(code_folding)
  if (code_folding != "none") {
    extra_dependencies <- append(extra_dependencies,
                                 list(html_dependency_codefolding_lua()))
    pandoc_args <- c(pandoc_args,
                     pandoc_metadata_arg("rmd_codefolding_lua", code_folding))
    lua_filters <- c(lua_filters, pkg_file_lua("codefolding.lua"))
  }

  pre_knit <- function(input, ...) {
    if (readme) {
      rmarkdown::render(input,
                        output_format = "github_document",
                        output_options = list(html_preview = FALSE),
                        output_file = "README.md",
                        output_dir = dirname(dirname(input)),
                        quiet = TRUE)
    }
  }

  pre_processor <- function(metadata, input_file, runtime, knit_meta,
                            files_dir, output_dir) {
    vignette_pre_processor(input_file, metadata)
  }

  base_format <- html_document(fig_width = fig_width,
                               fig_height = fig_height,
                               dev = dev,
                               fig_retina = NULL,
                               css = css,
                               theme = NULL,
                               highlight = highlight,
                               self_contained = self_contained,
                               extra_dependencies = extra_dependencies,
                               pandoc_args = pandoc_args,
                               ...)
  base_format$pandoc$lua_filters <- append(base_format$pandoc$lua_filters,
                                           lua_filters)
  output_format(
    knitr = NULL,
    pandoc = NULL,
    df_print = df_print,
    pre_knit = pre_knit,
    keep_md = keep_md,
    clean_supporting = self_contained,
    pre_processor = pre_processor,
    base_format = base_format
  )
}

vignette_pre_processor <- function(input_file, metadata = yaml_front_matter(input_file)) {
  if (getRversion() < 3.6)
    return()
  if (!getOption(o <- 'rmarkdown.html_vignette.check_title', !xfun::is_R_CMD_check()))
    return()
  title1 <- metadata[['title']]
  title2 <- tools::vignetteInfo(input_file)[['title']]
  # rmarkdown assumes UTF-8 only - tools::vignetteInfo uses readLines with
  # default OS encoding so issue on Windows for example
  # https://github.com/rstudio/rmarkdown/issues/1978
  Encoding(title2) <- "UTF-8"
  if (!identical(title1, title2)) warning(
    'The vignette title specified in \\VignetteIndexEntry{} is different from ',
    'the title in the YAML metadata. The former is "', title2, '", and the ',
    'latter is "', title1, '". If that is intentional, you may set options(', o,
    ' = FALSE) to suppress this check.', call. = FALSE
  )
  NULL
}
