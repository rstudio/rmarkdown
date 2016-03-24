#' Render multiple documents as a website
#'
#' Render all of the R Markdown documents within a directory as a website.
#'
#' @param input Website directory (or the name of a file within the directory)
#' @param output_format R Markdown format to convert to.
#' @param envir The environment in which the code chunks are to be evaluated
#'  during knitting (can use \code{\link{new.env}} to guarantee an empty new
#'  environment).
#' @param quiet \code{TRUE} to supress messages and other output.
#' @param encoding The encoding of the input file; see \code{\link{file}}.
#' @param ... Currently unused
#'
#' @details
#'
#' To render a group of R Markdown documents within a directory as a website
#' there are two requirements:
#' \itemize{
#'   \item{Include a file named \code{index.Rmd} which will serve as the site's
#'   home page.}
#'   \item{Add a \code{site: true} entry to the YAML metadata of \code{index.Rmd}}
#' }
#'
#' Once you've met these requirements you can call the \code{render_site} function
#' on the directory (or any Rmd file within the directory) to render all of the
#' R Markdown documents within the directory.
#'
#' @section Custom Site Generation:
#'
#' The default site generation function (\code{rmarkdown::default_site}) simply
#' renders all of the files in the input directory. You can however define a
#' custom site generator which has alternate behavior. There are two ways for
#' a custom site generator to be bound to from within \code{index.Rmd}:
#'
#' \itemize{
#'   \item{The active output format defines a "_site" function (e.g.
#'   \code{html_document_site}).}
#'   \item{The \code{site} metadata entry refers to a custom function (e.g.
#'   \code{mypackage::site}).}
#' }
#'
#' The site generation function should return a list with the following
#' elements:
#'
#' \itemize{
#'   \item{\code{name}} {The name for the website (e.g. the parent directory name).}
#'   \item{\code{output_dir} {The directory where the website output is written to
#'   (e.g. "." or "_site")}}
#'   \item{\code{render}} {An R function that can be called to generate the site.
#'   The function should accept the \code{output_format}, \code{envir}, \code{quiet},
#'   \code{encoding}, and \code{...} arguments.}
#' }
#'
#' See the source code of the \code{rmarkdown::default_site} function for a
#' simple example of a custom site generation function.
#'
#' @section Site Preview:
#'
#' Note that the \code{render_site} function can be used as a custom \code{knit}
#' handler within RStudio by adding the following to the YAML of any
#' document within a site directory:
#'
#' \code{knit: rmarkdown::render_site}
#'
#' This will result in the entire site being rendered each time Knit is
#' executed within RStudio.
#'
#' @export
render_site <- function(input = ".",
                        output_format = NULL,
                        envir = parent.frame(),
                        quiet = FALSE,
                        encoding = getOption("encoding"),
                        ...) {

  # normalize input (capture original input first)
  original_input <- input
  input <- input_as_dir(input)

  # find the site generator
  generator <- site_generator(input, output_format, encoding)
  if (is.null(generator))
    stop("No index.Rmd with site entry found.")

  # execute it
  generator$render(output_format = output_format,
                   envir = envir,
                   quiet = quiet,
                   encoding = encoding)

  # print the name of the index file (for RStudio Preview)
  if (!quiet) {
    # if the input was a filename use that as a base (otherwise
    # use index.Rmd)
    if (!dir_exists(original_input))
      output <- file_with_ext(basename(original_input), "html")
    else
      output <- "index.html"
    output <- file.path(input, generator$output_dir, output)
    message("\nOutput created: ", relative_to(getwd(), output))
  }
}

#' @rdname render_site
#' @export
site_generator <- function(input = ".",
                           output_format = NULL,
                           encoding = getOption("encoding")) {

  # normalize input
  input <- input_as_dir(input)

  # if we have an index.Rmd then check it's yaml for "site:"
  index <- file.path(input, "index.Rmd")
  if (file.exists(index)) {

    # read index.Rmd and extract the front matter
    index_lines <- read_lines_utf8(index, encoding)
    front_matter <- parse_yaml_front_matter(index_lines)
    if (!is.null(front_matter$site)) {

      # discover the site generator function (if "site" is a simple true
      # value then we use the default output format's name + _site). If
      # that function doesn't exist then just use rmarkdown::default_site,
      # which will just render all of the Rmd files in the directory
      if (isTRUE(front_matter$site)) {
        if (is.null(output_format))
          output_format <- default_output_format(index, encoding)$name
        site_function_name = paste0(output_format, "_site")
        create_site_generator <- tryCatch(
                                   eval(parse(text = site_function_name)),
                                   error = function(x) NULL)
        if (is.null(create_site_generator))
          create_site_generator <- rmarkdown::default_site;
      } else {
        create_site_generator <- eval(parse(text = front_matter$site))
      }

      # execute the create_site_generator function
      create_site_generator(input)

    } else {
      NULL  # no site: front matter
    }
  } else {
    NULL # no index.Rmd
  }
}

#' @rdname render_site
#' @export
default_site <- function(input, ...) {

  # define render function (use ... to gracefully handle future args)
  render <- function(output_format, envir, quiet, encoding, ...) {
    files <- list.files(input, pattern = "^[^_].*\\.Rmd$", full.names = TRUE)
    sapply(files, function(x) {
      # we suppress messages so that "Output created" isn't emitted
      # (which could result in RStudio previewing the wrong file)
      suppressMessages(
        rmarkdown::render(x,
                          output_format = output_format,
                          envir = envir,
                          quiet = quiet))
    })
  }

  # return site generator
  list(
    name = basename(normalize_path(input)),
    output_dir = input,
    render = render
  )
}

# utility function to ensure that 'input' is a valid directory
# (converts from file to parent directory as necessary)
input_as_dir <- function(input) {

  # convert from file to directory if necessary
  if (!dir_exists(input))
    input <- dirname(input)

  # ensure the input dir exists
  if (!dir_exists(input)) {
    stop("The specified directory '", normalize_path(input, mustWork = FALSE) +
           "' does not exist.", call. = FALSE)
  }

  # return it
  input
}


