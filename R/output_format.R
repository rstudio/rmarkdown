#' Define an R Markdown output format
#'
#' Define an R Markdown output format based on a combination of knitr and pandoc
#' options.
#'
#' @param knitr Knitr options for an output format (see
#'   \code{\link{knitr_options}})
#' @param pandoc Pandoc options for an output format (see
#'   \code{\link{pandoc_options}})
#' @param clean_supporting Cleanup any supporting files after conversion
#'   see \code{\link{render_supporting_files}}
#' @param format_filter An optional filter function that receives the
#'   \code{output_format}, \code{files_dir}, and \code{input_lines}.
#'   The filter can be called for it's side-effects (e.g. rendering
#'   supporting files to the \code{files_dir}) and can also return a
#'   modified format.
#' @param post_processor An optional post-processor function that
#'   receives the \code{input_file}, \code{output_file}, and
#'   \code{verbose} parmaeters.
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
                          clean_supporting = TRUE,
                          format_filter = NULL,
                          post_processor = NULL) {
  structure(list(knitr = knitr,
                 pandoc = pandoc,
                 clean_supporting = clean_supporting,
                 format_filter = format_filter,
                 post_processor = post_processor),
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
knitr_options_pdf <- function(fig_width, fig_height, fig_crop) {

  # default options
  opts_knit <- NULL
  opts_chunk <- list(dev = 'pdf',
                     fig.width = fig_width,
                     fig.height = fig_height)
  knit_hooks <- NULL

  # apply cropping if requested and we have pdfcrop
  crop <- fig_crop && !is_windows() && nzchar(Sys.which("pdfcrop"))
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
#' (+ features are added, - features are removed):
#'
#' \tabular{l}{
#' \code{+autolink_bare_uris} \cr
#' \code{+ascii_identifier} \cr
#' \code{+tex_math_single_backslash} \cr
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

    # caller additions or subtractions to the format
    extensions

  ), collapse = "")
}

#' Determine the default output format for an R Markdown document
#'
#' Read the YAML metadata (and any common output.yaml file) for the
#' document and return the output format that will be generated by
#' a call to \code{\link{render}}.
#'
#' @param input Input file (Rmd or plain markdown)
#' @param encoding The encoding of the input file; see \code{\link{file}}
#'
#' @return A named list with a \code{name} value containing the format
#'   name and an \code{options} value that is a list of rendering options.
#'
#' @details
#'
#' This function is useful for front-end tools that require additional
#' knowledge of the output to be produced by \code{\link{render}} (e.g. to
#' customize the preview experience).
#'
#' @export
default_output_format <- function(input, encoding = getOption("encoding")) {
  input_lines <- read_lines_utf8(input, encoding)
  output_format_from_yaml_front_matter(input_lines)
}


# Synthesize the output format for a document from it's YAML. If we can't
# find an output format then we just return html_document
output_format_from_yaml_front_matter <- function(input_lines,
                                                 output_options = NULL,
                                                 output_format_name = NULL) {

  # ensure input is the correct data type
  if (!is_null_or_string(output_format_name)) {
    stop("Unrecognized output format specified", call. = FALSE)
  }

  # parse the yaml
  yaml_front_matter <- parse_yaml_front_matter(input_lines)

  # default to no options
  output_format_options <- list()

  # parse common output.yaml if we have it
  if (file.exists("output.yaml"))
    common_output_format_yaml <- yaml::yaml.load_file("output.yaml")
  else
    common_output_format_yaml <- list()

  # parse output format from front-matter if we have it
  if (length(common_output_format_yaml) > 0 ||
      length(yaml_front_matter$output) > 0) {

    # alias the output format yaml
    output_format_yaml <- yaml_front_matter$output

    # merge against common output.yaml
    output_format_yaml <- merge_output_options(common_output_format_yaml,
                                               output_format_yaml)

    # if a named format was provided then try to find it
    if (!is.null(output_format_name)) {

      # if this is a named element of the list then use that
      if (output_format_name %in% names(output_format_yaml)) {

        output_format_options <- output_format_yaml[[output_format_name]]

      # otherwise this could be a heterogeneous list of characters and
      # lists so scan for an embedded list
      } else {
        for (format in output_format_yaml) {
          if (is.list(format) && !is.null(format[[output_format_name]]))
            output_format_options <- format[[output_format_name]]
        }
      }
    # no named format passed so take the first element
    } else {
      if (is.list(output_format_yaml[[1]])) {
        # check for named list
        if (nzchar(names(output_format_yaml)[[1]])) {
          output_format_name <- names(output_format_yaml)[[1]]
          output_format_options <- output_format_yaml[[1]]
        # nested named list
        } else {
          output_format_name <- names(output_format_yaml[[1]])[[1]]
          output_format_options <- output_format_yaml[[1]][[output_format_name]]
        }
      } else {
        output_format_name <- output_format_yaml[[1]]
      }
    }

  # no output formats defined in the file, just take the passed format
  # by name (or default to html_document if no named format was specified)
  } else {
    if (is.null(output_format_name))
      output_format_name <- "html_document"
  }

  # merge any output_options passed in the call to render
  if (!is.null(output_options)) {
    output_format_options <- merge_output_options(output_format_options,
                                                  output_options)
  }

  # return the format name and options
  list(name = output_format_name,
       options = output_format_options)
}

create_output_format <- function(name, options) {

  # lookup the function
  output_format_func <- eval(parse(text = name))
  if (!is.function(output_format_func))
    stop("YAML output format must evaluate to a function", call. = FALSE)

  # call the function
  output_format <- do.call(output_format_func, options)
  if (!is_output_format(output_format))
    stop("Format is not of class rmarkdown_output_format", call. = FALSE)

  # return the format
  output_format
}

is_output_format <- function(x) {
  inherits(x, "rmarkdown_output_format")
}

parse_yaml_front_matter <- function(input_lines) {

  partitions <- partition_yaml_front_matter(input_lines)
  if (!is.null(partitions$front_matter)) {
    front_matter <- partitions$front_matter
    if (length(front_matter) > 2) {
      front_matter <- front_matter[2:length(front_matter)-1]
      yaml::yaml.load(paste(front_matter, collapse="\n"))
    }
    else
      list()
  }
  else
    list()
}

partition_yaml_front_matter <- function(input_lines) {

  # is there yaml front matter?
  delimiters <- grep("^---\\s*$", input_lines)
  if (length(delimiters) >= 2 && (delimiters[2] - delimiters[1] > 1)) {

    front_matter <- input_lines[(delimiters[1]):(delimiters[2])]

    input_body <- c()

    if (delimiters[1] > 1)
      input_body <- c(input_body,
                      input_lines[1:delimiters[1]-1])

    if (delimiters[2] < length(input_lines))
      input_body <- c(input_body,
                      input_lines[-(1:delimiters[2])])

    list(front_matter = front_matter,
         body = input_body)
  }
  else {
    list(front_matter = NULL,
         body = input_lines)
  }
}

merge_output_options <- function(base_options, overlay_options) {

  # if either one of these is a character vector then normalize to a named list
  normalize_list <- function(target_list) {
    if (is.character(target_list)) {
      new_list <- list()
      for (name in target_list)
        new_list[[name]] <- list()
      new_list
    } else {
      target_list
    }
  }

  base_options <- normalize_list(base_options)
  overlay_options <- normalize_list(overlay_options)

  merge_lists(base_options, overlay_options)
}


