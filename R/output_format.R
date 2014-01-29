#' Define an R Markdown output format
#'
#' Define an R Markdown output format based on a combination of knitr and pandoc
#' options.
#'
#' @param to Pandoc format to convert to
#' @param knitr List of knitr options and hooks. Valid list elements include
#'   \code{\link[knitr:opts_knit]{opts_knit}},
#'   \code{\link[knitr:opts_chunk]{opts_chunk}}, and
#'   \code{\link[knitr:knit_hooks]{knit_hooks}}.
#' @param pandoc Character vector of pandoc command line arguments
#' @param filter An optional filter function that receieves the format and lines
#'   of the input file as input and can return a modified format.
#'
#' @return An R Markdown output format definition that can be passed to
#'   \code{\link{render}}.
#'
#' @seealso \link{render}
#'
#' @export
output_format <- function(to, knitr, pandoc, filter = NULL) {
  structure(list(to = to,
                 knitr = knitr,
                 pandoc = pandoc,
                 filter = filter),
            class = "rmarkdown_output_format")
}


# Synthesize the output format for a document from it's YAML. If we can't
# find an output format then we just return html_document
output_format_from_yaml <- function(input_lines) {

  # parse output format from front-matter if we have it
  output_format_yaml <- parse_output_format_yaml(input_lines)
  if (!is.null(output_format_yaml)) {

    # verify we have a format
    if (is.null(output_format_yaml$format))
      stop("YAML output_ field must include a format", call. = FALSE)

    # note the format and remove it from the list
    format <- output_format_yaml$format
    output_format_yaml$format <- NULL

    # lookup the function
    output_format_func <- eval(parse(text = format))
    if (!is.function(output_format_func))
      stop("YAML output format must evaluate to a function", call. = FALSE)

    # call the function
    output_format <- do.call(output_format_func, output_format_yaml)
    if (!inherits(output_format, "rmarkdown_output_format"))
      stop("Format is not of class rmarkdown_output_format", call. = FALSE)

    # return the format
    output_format
  }
  else {
    html_document()
  }
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
