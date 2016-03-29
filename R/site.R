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
#' @return The name of the site output file (relative to the input directory).
#'
#' @export
render_site <- function(input = ".",
                        output_format = "all",
                        envir = parent.frame(),
                        quiet = FALSE,
                        encoding = getOption("encoding"),
                        ...) {

  # capture original input
  original_input <- input

  # normalize to a directory
  input <- input_as_dir(input)

  # if it's a file then capture that
  input_file <- NULL
  if (!dir_exists(original_input))
    input_file <- original_input

  # find the site generator
  generator <- site_generator(input, output_format, encoding)
  if (is.null(generator))
    stop("No website index file with 'site' metadata found.")

  # execute it
  generator$render(input_file = input_file,
                   output_format = output_format,
                   envir = envir,
                   quiet = quiet,
                   encoding = encoding)

  # compute the name of the output file. if the input was a filename
  # use that as a base (otherwise use index.html)
  if (!dir_exists(original_input))
    output <- file_with_ext(basename(original_input), "html")
  else
    output <- "index.html"
  output <- file.path(input, generator$output_dir, output)
  output <- normalized_relative_to(input, output)

  # print the name of the output file (for RStudio Preview)
  if (!quiet)
    message("\nOutput created: ", output)

  # return it invisibly
  invisible(output)
}

#' @rdname render_site
#' @export
site_generator <- function(input = ".",
                           output_format = NULL,
                           encoding = getOption("encoding")) {

  # normalize input
  input <- input_as_dir(input)

  # if we have an index.Rmd (or .md) then check it's yaml for "site:"
  index <- file.path(input, "index.Rmd")
  if (!file.exists(index))
    index <- file.path(input, "index.md")
  if (file.exists(index)) {

    # read index.Rmd and extract the front matter
    index_lines <- read_lines_utf8(index, encoding)
    front_matter <- parse_yaml_front_matter(index_lines)

    # is there a custom site generator function?
    if (!is.null(front_matter$site)) {
      create_site_generator <- eval(parse(text = front_matter$site))
      create_site_generator(input, encoding)

    # is there a "_site.yml"?
    } else if (file.exists(site_config_file(input))) {
      rmarkdown::default_site(input, encoding)

    # no custom site generator or "_site.yml"
    } else {
      NULL
    }

  # no index.Rmd or index.md
  } else {
    NULL
  }
}

#' @rdname render_site
#' @export
site_config <- function(input, encoding = getOption("encoding")) {

  # normalize input
  input <- input_as_dir(input)

  # check for config file
  config_file <- site_config_file(input)
  if (file.exists(config_file)) {

    # parse the yaml
    config_lines <- read_lines_utf8(config_file, encoding)
    config <- yaml_load_utf8(config_lines)
    if (!is.list(config))
      config <- list()

    # provide defaults if necessary
    if (is.null(config$name))
      config$name <- basename(normalize_path(input))
    if (is.null(config$output_dir))
      config$output_dir <- "_site"

    # return config
    config

  # no _site.yml
  } else {
    NULL
  }
}


#' @rdname render_site
#' @export
default_site <- function(input, encoding = getOption("encoding"), ...) {

  # get the site config
  config <- site_config(input, encoding)

  # define render function (use ... to gracefully handle future args)
  render <- function(input_file,
                     output_format,
                     envir,
                     quiet,
                     encoding, ...) {

    # track outputs
    outputs <- c()

    # see if this is an incremental render
    incremental <- !is.null(input_file)

    # files list is either a single file (for incremental) or all
    # file within the input directory
    if (incremental)
      files <- input_file
    else {
      # render all .Rmd and .md files that don't start with "_" (note that
      # don't do this recursively because rmarkdown in general handles
      # applying common options/elements across subdirectories poorly)
      files <- list.files(input, pattern = "^[^_].*\\.R?md$", full.names = TRUE)
    }
    sapply(files, function(x) {
      # we suppress messages so that "Output created" isn't emitted
      # (which could result in RStudio previewing the wrong file)
      output <- suppressMessages(
        rmarkdown::render(x,
                          output_format = output_format,
                          output_options = list(lib_dir = "lib",
                                                self_contained = FALSE),
                          envir = envir,
                          quiet = quiet)
      )

      # add to global list of outputs
      outputs <<- c(outputs, output)

      # check for files dir and add that as well
      sidecar_files_dir <- knitr_files_dir(output)
      files_dir_info <- file.info(sidecar_files_dir)
      if (isTRUE(files_dir_info$isdir))
        outputs <<- c(outputs, sidecar_files_dir)
    })

    # do we have a relative output directory? if so then remove,
    # recreate, and copy outputs to it (we don't however remove
    # it for incremental builds)
    if (config$output_dir != '.') {

      # remove and recreate output dir if necessary
      output_dir <- file.path(input, config$output_dir)
      if (file.exists(output_dir)) {
        if (!incremental) {
          unlink(output_dir, recursive = TRUE)
          dir.create(output_dir)
        }
      } else {
        dir.create(output_dir)
      }

      # move outputs
      for (output in outputs) {
        output_dest <- file.path(output_dir, basename(output))
        if (dir_exists(output_dest))
          unlink(output_dest, recursive = TRUE)
        file.rename(output, output_dest)
      }

      # copy lib dir a directory at a time (allows it to work with incremental)
      lib_dir <- file.path(input, "lib")
      output_lib_dir <- file.path(output_dir, "lib")
      if (!file.exists(output_lib_dir))
        dir.create(output_lib_dir)
      libs <- list.files(lib_dir)
      for (lib in libs)
        file.copy(file.path(lib_dir, lib), output_lib_dir, recursive = TRUE)
      unlink(lib_dir, recursive = TRUE)

    # no output directory
    } else {
      output_dir <- input
    }

    # get the original file list (we'll need it to apply includes)
    all_files <- list.files(input, all.files = TRUE)

    # excludes:
    #   - known source/data extensions
    #   - anything that starts w/ '.' or '_'
    #   - rsconnect directory
    #   - user excludes
    extensions <- c("R", "r", "S", "s",
                    "Rmd", "rmd", "md", "Rmarkdown", "rmarkdown",
                    "Rproj", "rproj",
                    "RData", "rdata", "rds")
    extensions_regex <- glob2rx(paste0("*.", extensions))
    excludes <- c("^rsconnect$", "^\\..*$", "^_.*$",
                  extensions_regex,
                  glob2rx(config$exclude))
    files <- all_files
    for (exclude in excludes)
      files <- files[!grepl(exclude, files)]

    # allow back in anything specified as an explicit "include"
    includes <- glob2rx(config$include)
    for (include in includes) {
      include_files <- all_files[grepl(include, all_files)]
      files <- unique(c(files, include_files))
    }

    # copy to output_dir if necessary
    if (config$output_dir != '.') {
      output_dir <- file.path(input, config$output_dir)
      file.copy(from = file.path(input, files),
                to = output_dir,
                recursive = TRUE)
    }
  }

  # return site generator
  list(
    name = config$name,
    output_dir = config$output_dir,
    render = render
  )
}

# utility function to ensure that 'input' is a valid directory
# (converts from file to parent directory as necessary)
input_as_dir <- function(input) {

  # ensure the input dir exists
  if (!file.exists(input)) {
    stop("The specified directory '", normalize_path(input, mustWork = FALSE),
         "' does not exist.", call. = FALSE)
  }

  # convert from file to directory if necessary
  if (!dir_exists(input))
    input <- dirname(input)

  # return it
  input
}


site_config_file <- function(input) {
  file.path(input, "_site.yml")
}


