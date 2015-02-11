#' Base output format for HTML-based output formats
#' 
#' Creates an HTML base output format suitable for passing as the 
#' \code{base_format} argument of the \code{\link{output_format}} function.
#' 
#' @inheritParams html_document
#' 
#' @param dependency_resolver A dependency resolver
#' @param copy_resources Copy resources
#' @param extra_dependencies Extra dependencies
#' @param bootstrap_compatible Bootstrap compatible
#' 
#' @return HTML base output format.
#' 
#' @export
html_document_base <- function(smart = TRUE,
                               theme = NULL,
                               self_contained = TRUE,
                               lib_dir = NULL,
                               mathjax = "default",
                               pandoc_args = NULL,
                               template = "default",
                               dependency_resolver = NULL,
                               copy_resources = FALSE,
                               extra_dependencies = NULL,
                               bootstrap_compatible = FALSE,
                               ...) {
  
  # default for dependency_resovler
  if (is.null(dependency_resolver))
    dependency_resolver <- html_dependency_resolver
  
  args <- c()

  # smart quotes, etc.
  if (smart)
    args <- c(args, "--smart")

  # no email obfuscation
  args <- c(args, "--email-obfuscation", "none")

  # self contained document
  if (self_contained) {
    if (copy_resources)
      stop("Local resource copying is incompatible with self-contained documents.")
    validate_self_contained(mathjax)
    args <- c(args, "--self-contained")
  }

  # custom args
  args <- c(args, pandoc_args)

  preserved_chunks <- character()

  output_dir <- ""

  pre_processor <- function (metadata, input_file, runtime, knit_meta,
                             files_dir, output_dir) {

    args <- c()

    # use files_dir as lib_dir if not explicitly specified
    if (is.null(lib_dir))
      lib_dir <<- files_dir

    # copy supplied output_dir (for use in post-processor)
    output_dir <<- output_dir

    # handle theme
    if (!is.null(theme)) {
      theme <- match.arg(theme, themes())
      if (identical(theme, "default"))
        theme <- "bootstrap"
      args <- c(args, "--variable", paste("theme:", theme, sep=""))
    }

    # resolve and inject extras, including dependencies specified by the format
    # and dependencies specified by the user (via extra_dependencies)
    format_deps <- list()
    format_deps <- append(format_deps, extra_dependencies)
    if (!is.null(theme)) {
      format_deps <- append(format_deps, list(html_dependency_jquery(),
                                              html_dependency_bootstrap(theme)))
    }
    else if (isTRUE(bootstrap_compatible) && identical(runtime, "shiny")) {
      # If we can add bootstrap for Shiny, do it
      format_deps <- append(format_deps,
                            list(html_dependency_bootstrap("bootstrap")))
    }

    extras <- html_extras_for_document(knit_meta, runtime, dependency_resolver,
                                       format_deps)
    args <- c(args, pandoc_html_extras_args(extras, self_contained, lib_dir,
                                            output_dir))

    # mathjax
    args <- c(args, pandoc_mathjax_args(mathjax,
                                        template,
                                        self_contained,
                                        lib_dir,
                                        output_dir))

    # The input file is converted to UTF-8 from its native encoding prior
    # to calling the preprocessor (see ::render)
    input_str <- readLines(input_file, warn = FALSE, encoding = "UTF-8")
    preserve <- extractPreserveChunks(input_str)
    if (!identical(preserve$value, input_str))
      writeLines(preserve$value, input_file, useBytes = TRUE)
    preserved_chunks <<- preserve$chunks

    args
  }

  post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    # if there are no preserved chunks to restore and no resource to copy then no
    # post-processing is necessary
    if (length(preserved_chunks) == 0 && !isTRUE(copy_resources) && self_contained)
      return(output_file)

    # read the output file
    output_str <- readLines(output_file, warn = FALSE, encoding = "UTF-8")

    # if we preserved chunks, restore them
    if (length(preserved_chunks) > 0)
      output_str <- restorePreserveChunks(output_str, preserved_chunks)

    # The copy_resources flag copies all the resources referenced in the
    # document to its supporting files directory, and rewrites the document to
    # use the copies from that directory.
    if (copy_resources) {
      relative_lib <- normalized_relative_to(output_dir, lib_dir)
      resource_copier <- function(node, att) {
        # get the resource referenced, and skip this node if it doesn't 
        # reference a resource
        src <- node$attributes[att]
        if (is.null(src) || is.na(src)) {
          return(node)
        }
        in_file <- utils::URLdecode(src)
        
        # only process the file if (a) it isn't already in the library, and (b)
        # it exists on the local file system (this also excludes external
        # resources such as "http://foo/bar.png")
        if (length(in_file) &&
            substr(src, 1, nchar(lib_dir) + 1) != file.path(relative_lib, "") &&
            file.exists(in_file)) {

          # check to see if it's already in the library (by absolute path)
          res_src <- normalized_relative_to(lib_dir, in_file) 
          if (identical(res_src, normalizePath(in_file, winslash = "/"))) {
            # not inside the library, copy it there
            target_dir <- if (dirname(in_file) == ".")  
              lib_dir 
            else 
              file.path(lib_dir, dirname(in_file))
            dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
            target_res_file <- file.path(target_dir, basename(in_file))
            
            # copy the file to the library
            if (!file.exists(target_res_file)) {
              file.copy(in_file, target_res_file)
            }
            
            res_src <- file.path(relative_lib, src)
          } else {
            # inside the library, fix up the URL
            res_src <- file.path(relative_lib, res_src)
          }
          
          # replace the reference in the document
          output_str <<- sub(paste("(\\s+", att, "\\s*=\\s*['\"])", 
                                  escape_regex_metas(src), "(['\"])", 
                                  sep = ""),
                             paste("\\1", res_src, "\\2", sep = ""), 
                             output_str)
        }
        node
      }
      
      XML::htmlTreeParse(file = output_str, asText = TRUE, handlers = list(
          img    = function(node)  { resource_copier(node, "src")  },
          link   = function(node)  { resource_copier(node, "href") },
          iframe = function(node)  { resource_copier(node, "src")  },
          object = function(node)  { resource_copier(node, "data") },
          script = function(node)  { resource_copier(node, "src")  },
          audio  = function(node)  { resource_copier(node, "src")  },
          video  = function(node)  { resource_copier(node, "src")  },
          embed  = function(node)  { resource_copier(node, "src")  }
        ))

    } else if (!self_contained) {
      # if we're not self-contained, find absolute references to the output
      # directory and replace them with relative ones
      image_relative <- function(img_src, src) {
        in_file <- utils::URLdecode(src)
        if (length(in_file) && file.exists(in_file)) {
          img_src <- sub(
            src, utils::URLencode(relative_to(output_dir, in_file)), img_src)
        }
        img_src
      }
      output_str <- process_images(output_str, image_relative)
    }

    writeLines(output_str, output_file, useBytes = TRUE)
    output_file
  }

  output_format(
    knitr = NULL,
    pandoc = pandoc_options(to = "html", from = NULL, args = args),
    keep_md = FALSE,
    clean_supporting = FALSE,
    pre_processor = pre_processor,
    post_processor = post_processor
  )
}

