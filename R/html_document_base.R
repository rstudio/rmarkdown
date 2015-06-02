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
  
  intermediates_generator <- function(original_input, encoding, 
                                      intermediates_dir) {
    # copy intermediates; skip web resources if not self contained (pandoc can
    # create references to web resources without the file present)
    return(copy_render_intermediates(original_input, encoding, 
                                     intermediates_dir, !self_contained))
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
      
      # ensure the lib directory exists
      dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
      relative_lib <- normalized_relative_to(output_dir, lib_dir)
      
      res_replacements <- c()
      
      resource_copier <- function(node, att, src, idx) {
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
          
          # replace the reference in the document and adjust the offset
          if (!identical(src, res_src)) {
            res_replacements <<- c(res_replacements, list(list(
              pos = idx,
              len = nchar(src),
              text = res_src)))
          }
        }
      }
      
      # parse the HTML and copy the resources found
      output_str <- paste(output_str, collapse = "\n")
      call_resource_attrs(output_str, resource_copier)
      
      # rewrite the HTML to refer to the copied resources
      if (length(res_replacements) > 0) {
        ch_pos <- 1
        new_output_str <- ""
        
        # we do all replacements in bytes for performance reasons--mark the 
        # output with byte encoding temporarily 
        prev_encoding <- Encoding(output_str)
        Encoding(output_str) <- "bytes"
        Encoding(new_output_str) <- "bytes"
        
        for (res_rep in seq_along(res_replacements)) {
          rep <- res_replacements[[res_rep]]
          
          # the text from the last replacement to the current one
          before <- substr(output_str, ch_pos, rep$pos - 1)
          ch_pos <- rep$pos + rep$len
          
          # the text from the current replacement to the end of the output,
          # if applicable
          after <- if (res_rep == length(res_replacements))
            substring(output_str, ch_pos)
          else 
            ""
          
          # compose the next segment of the output from the text between
          # replacements and the current replacement text
          new_output_str <- paste(new_output_str, before, rep$text, after,
                                  sep = "")
        }
        output_str <- new_output_str
        Encoding(output_str) <- prev_encoding
      }
      
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
    intermediates_generator = intermediates_generator,
    post_processor = post_processor
  )
}

