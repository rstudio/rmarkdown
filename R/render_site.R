
#' @noRd
#' @export
render_site <- function(input = ".",
                        output_format = "all",
                        envir = parent.frame(),
                        quiet = FALSE,
                        encoding = getOption("encoding")) {

  # capture original input
  original_input <- input

  # normalize to a directory
  input <- input_as_dir(input)

  # if it's a file then capture that and force output_format to be NULL
  # (to only render a single format for incremental/previewing)
  input_file <- NULL
  if (!dir_exists(original_input)) {
    input_file <- original_input
    if (output_format == "all")
      output_format <- NULL
  }

  # find the site generator
  generator <- site_generator(input, output_format, encoding)
  if (is.null(generator))
    stop("No site generator found.")

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

  # return it invisibly
  invisible(output)
}

#' @noRd
#' @export
clean_site <- function(input = ".", preview = FALSE, quiet = FALSE,
                       encoding = getOption("encoding")) {

  # normalize to a directory
  input <- input_as_dir(input)

  # find the site generator
  generator <- site_generator(input = input,
                              output_format = NULL,
                              encoding = encoding)
  if (is.null(generator))
    stop("No site generator found.")

  # get the files to be cleaned
  files <- generator$clean()

  # if it's just a preview then return the files, otherwise
  # actually remove the files
  if (preview)
    files
  else {
    if (!quiet) {
      cat("Removing files: \n")
      cat(paste(paste0(" ", files), collapse = "\n"))
    }
    unlink(file.path(input, files), recursive = TRUE)
  }
}

#' @noRd
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

      default_site(input, encoding)

    # no custom site generator or "_site.yml"
    } else {
      NULL
    }

  # no index.Rmd or index.md
  } else {
    NULL
  }
}

# helper function to get the site configuration as an R list
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

# default site implementation (can be overridden by custom site generators)
default_site <- function(input, encoding = getOption("encoding"), ...) {

  # get the site config
  config <- site_config(input, encoding)
  if (is.null(config))
    stop("No site configuration (_site.yml) file found.")

  # helper function to get all input files. includes all .Rmd and
  # .md files that don't start with "_" (note that we don't do this
  # recursively because rmarkdown in general handles applying common
  # options/elements across subdirectories poorly)
  input_files <- function() {
    list.files(input, pattern = "^[^_].*\\.R?md$")
  }

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
      files <- file.path(input, input_files())
    }
    sapply(files, function(x) {
      # we suppress messages so that "Output created" isn't emitted
      # (which could result in RStudio previewing the wrong file)
      output <- suppressMessages(
        rmarkdown::render(x,
                          output_format = output_format,
                          output_options = list(lib_dir = "site_libs",
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

        # don't move it if it's a _files dir that has a _cache dir
        if (grepl("^.*_files$", output)) {
          cache_dir <- gsub("_files$", "_cache", output)
          if (dir_exists(cache_dir))
            next;
        }

        output_dest <- file.path(output_dir, basename(output))
        if (dir_exists(output_dest))
          unlink(output_dest, recursive = TRUE)
        file.rename(output, output_dest)
      }

      # copy lib dir a directory at a time (allows it to work with incremental)
      lib_dir <- file.path(input, "site_libs")
      output_lib_dir <- file.path(output_dir, "site_libs")
      if (!file.exists(output_lib_dir))
        dir.create(output_lib_dir)
      libs <- list.files(lib_dir)
      for (lib in libs)
        file.copy(file.path(lib_dir, lib), output_lib_dir, recursive = TRUE)
      unlink(lib_dir, recursive = TRUE)

      # copy other files
      copy_site_resources(input, encoding)
    }

    # Print output created for rstudio preview
    if (!quiet) {
      # determine output file
      output_file <- ifelse(is.null(input_file),
                            "index.html",
                            file_with_ext(basename(input_file), "html"))
      if (config$output_dir != ".")
        output_file <- file.path(config$output_dir, output_file)
      message("\nOutput created: ", output_file)
    }
  }

  # define clean function
  clean <- function() {

    # build list of generated files
    generated <- c()

    # enumerate rendered markdown files
    files <- input_files()

    # get html files
    html_files <- file_with_ext(files, "html")

    # _files peers are always removed (they could be here due to
    # output_dir == "." or due to a _cache existing for the page)
    html_supporting <- paste0(knitr_files_dir(html_files), '/')
    generated <- c(generated, html_supporting)

    # _cache peers are always removed
    html_cache <- paste0(knitr_root_cache_dir(html_files), '/')
    generated <- c(generated, html_cache)

    # for rendering in the current directory we need to eliminate
    # output files for our inputs (including _files) and the lib dir
    if (config$output_dir == ".") {

      # .html peers
      generated <- c(generated, html_files)

      # site_libs dir
      generated <- c(generated, "site_libs/")

    # for an explicit output_dir just remove the directory
    } else {
      generated <- c(generated, paste0(config$output_dir, '/'))
    }

    # filter out by existence
    generated[file.exists(file.path(input, generated))]
  }

  # return site generator
  list(
    name = config$name,
    output_dir = config$output_dir,
    render = render,
    clean = clean
  )
}

# utility function to copy all files into the _site directory
copy_site_resources <- function(input, encoding = getOption("encoding")) {

  # get the site config
  config <- site_config(input, encoding)

  if (config$output_dir != ".") {

    # get the list of files
    files <- copyable_site_resources(input = input,
                                     config = config,
                                     recursive = FALSE,
                                     encoding = encoding)

    # perform the copy
    output_dir <- file.path(input, config$output_dir)
    file.copy(from = file.path(input, files),
              to = output_dir,
              recursive = TRUE)
  }
}

# utility function to list the files that should be copied
copyable_site_resources <- function(input,
                                    config = site_config(input, encoding),
                                    recursive = FALSE,
                                    encoding = getOption("encoding")) {

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
  extensions_regex <- utils::glob2rx(paste0("*.", extensions))
  excludes <- c("^rsconnect$", "^\\..*$", "^_.*$", "^.*_cache$",
                extensions_regex,
                utils::glob2rx(config$exclude))
  files <- all_files
  for (exclude in excludes)
    files <- files[!grepl(exclude, files)]

  # allow back in anything specified as an explicit "include"
  includes <- utils::glob2rx(config$include)
  for (include in includes) {
    include_files <- all_files[grepl(include, all_files)]
    files <- unique(c(files, include_files))
  }

  # if this is recursive then we need to blow out the directories
  if (recursive) {
    recursive_files <- c()
    for (file in files) {
      file_path <- file.path(input, file)
      if (dir_exists(file_path)) {
        dir_files <- file.path(list.files(file_path,
                                          full.names = FALSE,
                                          recursive = TRUE))
        dir_files <- file.path(file, dir_files)
        recursive_files <- c(recursive_files, dir_files)
      } else {
        recursive_files <- c(recursive_files, file)
      }
    }
    recursive_files
  } else {
    files
  }
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

# get the path to the site config file
site_config_file <- function(input) {
  file.path(input, "_site.yml")
}


