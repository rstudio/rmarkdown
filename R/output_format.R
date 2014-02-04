#' Define an R Markdown output format
#'
#' Define an R Markdown output format based on a combination of knitr and pandoc
#' options.
#'
#' @param knitr Knitr options for an output format (see
#'   \code{\link{knitr_options}})
#' @param pandoc Pandoc options for an output format (see
#'   \code{\link{pandoc_options}})
#' @param clean.files Cleanup any supporting files after conversion
#'   see \code{\link{copy_supporting_files}}
#' @param filter An optional filter function that receives the output format,
#'   output file, and lines of the input file. The filter can be called for
#'   it's side-effects (e.g. copying files to the output directory) and can
#'   also return a modified format.
#'
#' @return An R Markdown output format definition that can be passed to
#'   \code{\link{render}}.
#'
#' @seealso \link{render}, \link{knitr_options}, \link{pandoc_options}
#'
#' @examples
#' \dontrun{
#' output_format(knitr = knitr_options(opts_chunk = list(dev = 'png')),
#'               pandoc = pandoc_options(to = "html"))
#' }
#'
#' @export
output_format <- function(knitr,
                          pandoc,
                          clean.supporting = TRUE,
                          filter = NULL) {
  structure(list(knitr = knitr,
                 pandoc = pandoc,
                 clean.supporting = clean.supporting,
                 filter = filter),
            class = "rmarkdown_output_format")
}

#' Knitr options for an output format
#'
#' Define the knitr options for an R Markdown output format.
#'
#' @param opts_knit List of package level knitr options (see
#'   \code{\link[knitr:opts_knit]{opts_knit}})
#' @param opts_chunk List of chunk level knitr options (see
#'   \code{\link[knitr:opts_chunk]{opts_chunk}})
#' @param knit_hooks List of hooks for R code chunks, inline R code, and output
#'   (see \code{\link[knitr:knit_hooks]{knit_hooks}})
#'
#' @return An list that can be passed as the \code{knitr} argument of the
#'   \code{\link{output_format}} function.
#'
#' @seealso \link{output_format}
#'
#' @export
knitr_options <- function(opts_knit = NULL,
                          opts_chunk = NULL,
                          knit_hooks = NULL) {
  list(opts_knit = opts_knit,
       opts_chunk = opts_chunk,
       knit_hooks = knit_hooks)
}

#' Knitr options for a PDF output format
#'
#' Define knitr options for an R Markdown output format that creates PDF output.
#'
#' @inheritParams html_document
#' @inheritParams pdf_document
#'
#' @return An list that can be passed as the \code{knitr} argument of the
#'   \code{\link{output_format}} function.
#'
#' @seealso \link{knitr_options}, \link{output_format}
#'
#' @export
knitr_options_pdf <- function(fig.width, fig.height, fig.crop) {

  # default options
  opts_knit <- NULL
  opts_chunk <- list(dev = ifelse(capabilities('cairo'), 'cairo_pdf', 'pdf'),
                     fig.width = fig.width,
                     fig.height = fig.height)
  knit_hooks <- NULL

  # apply cropping if requested and we have pdfcrop
  crop <- fig.crop && !is_windows() && nzchar(Sys.which("pdfcrop"))
  if (crop) {
    knit_hooks = list(crop = knitr::hook_pdfcrop)
    opts_chunk$crop = TRUE
  }

  # return options
  knitr_options(opts_knit = opts_knit,
                opts_chunk = opts_chunk,
                knit_hooks = knit_hooks)
}


#' Pandoc options for an output format
#'
#' Define the pandoc options for an R Markdown output format.
#'
#' @param to Pandoc format to convert to
#' @param from Pandoc format to convert from
#' @param args Character vector of command line arguments to pass to pandoc
#'
#' @return An list that can be passed as the \code{pandoc} argument of the
#'   \code{\link{output_format}} function.
#'
#' @details The \code{from} argument should be used very cautiously as it's
#'   important for users to be able to rely on a stable definition of supported
#'   markdown extensions.
#'
#' @seealso \link{output_format}, \link{rmarkdown_format}
#'
#' @export
pandoc_options <- function(to,
                           from = rmarkdown_format(),
                           args = NULL) {
  list(to = to,
       from = from,
       args = args)
}

#' R Markdown input format definition
#'
#' Compose a pandoc markdown input definition for R Markdown that can be
#' passed as the \code{from} argument of \link{pandoc_options}.
#'
#' @param extensions Markdown extensions to be added or removed from the
#' default definition of R Markdown.
#'
#' @return Pandoc markdown format specification
#'
#' @details
#'
#' By default R Markdown is defined as all pandoc markdown extensions with
#' the following tweaks for backward compatibility with the markdown package
#' and to avoid some current bugs of pandoc (+ features are added, - features
#' are removed):
#'
#' \tabular{l}{
#' \code{+autolink_bare_uris} \cr
#' \code{+ascii_identifier} \cr
#' \code{+tex_math_single_backslash} \cr
#' \code{-markdown_in_html_blocks} \cr
#' }
#'
#'
#' For more on pandoc markdown see the \href{http://johnmacfarlane.net/pandoc/demo/example9/pandocs-markdown.html}{pandoc markdown specification}.
#'
#' @examples
#' \dontrun{
#' rmarkdown_format("-implicit_figures")
#' }
#'
#' @seealso \link{output_format}, \link{pandoc_options}
#'
#' @export
rmarkdown_format <- function(extensions = NULL) {

  paste(c(

    # core pandoc markdown (all extensions enabled)
    "markdown",

    # additional github flavored markdown extensions for
    # compatibility with the markdown package
    "+autolink_bare_uris",
    "+ascii_identifiers",
    "+tex_math_single_backslash",

    # we've found this to be problematic in the face of HTML that
    # contains indentation (it gets convereted to a code block)
    "-markdown_in_html_blocks",

    # caller additions or subtractions to the format
    extensions

  ), collapse = "")
}

# Synthesize the output format for a document from it's YAML. If we can't
# find an output format then we just return html_document
output_format_from_yaml <- function(output_format_expr, input_lines) {

  # ensure input is the correct data type
  if (!is_null_or_string(output_format_expr)) {
    stop("Unrecognized output format specified", call. = FALSE)
  }

  # default to no args
  output_format_args <- list()

  # parse output format from front-matter if we have it
  output_format_yaml <- parse_output_format_yaml(input_lines)
  if (length(output_format_yaml) > 0) {

    # if a named format was provided then try to find it
    if (!is.null(output_format_expr)) {

      # if this is a named element of the list then use that
      if (output_format_expr %in% names(output_format_yaml)) {

        output_format_args <- output_format_yaml[[output_format_expr]]

      # otherwise this could be a heterogeneous list of characters and
      # lists so scan for an embedded list
      } else {
        for (format in output_format_yaml) {
          if (is.list(format) && !is.null(format[[output_format_expr]]))
            output_format_args <- format[[output_format_expr]]
        }
      }
    # no named format passed so take the first element
    } else {
      if (is.list(output_format_yaml[[1]])) {
        # check for named list
        if (nzchar(names(output_format_yaml)[[1]])) {
          output_format_expr <- names(output_format_yaml)[[1]]
          output_format_args <- output_format_yaml[[1]]
        # nested named list
        } else {
          output_format_expr <- names(output_format_yaml[[1]])[[1]]
          output_format_args <- output_format_yaml[[1]][[output_format_expr]]
        }
      } else {
        output_format_expr <- output_format_yaml[[1]]
      }
    }

  # no output formats defined in the file, just take the passed format
  # by name (or default to html_document if no named format was specified)
  } else {
    if (is.null(output_format_expr))
      output_format_expr <- "html_document"
  }

  # lookup the function
  output_format_func <- eval(parse(text = output_format_expr))
  if (!is.function(output_format_func))
    stop("YAML output format must evaluate to a function", call. = FALSE)

  # call the function
  output_format <- do.call(output_format_func, output_format_args)
  if (!is_output_format(output_format))
    stop("Format is not of class rmarkdown_output_format", call. = FALSE)

  # return the format
  output_format
}

is_output_format <- function(x) {
  inherits(x, "rmarkdown_output_format")
}

parse_output_format_yaml <- function(input_lines) {

  # is there yaml front matter?
  delimiters <- grep("^---\\s*$", input_lines)
  if (length(delimiters) >= 2 && (delimiters[2] - delimiters[1] > 1)) {

    # attempt to parse the yaml
    front_matter <- input_lines[(delimiters[1]+1):(delimiters[2]-1)]
    front_matter_yaml <- yaml::yaml.load(paste(front_matter, collapse="\n"))
    if (!is.null(front_matter_yaml))
      front_matter_yaml$output
    else
      NULL
  }
  else {
    NULL
  }
}
