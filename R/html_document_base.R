# base output format for HTML-based output formats
html_document_base <- function(smart = TRUE,
                               theme = NULL,
                               self_contained = TRUE,
                               lib_dir = NULL,
                               mathjax = "default",
                               pandoc_args = NULL,
                               template = "default",
                               dependency_resolver = html_dependency_resolver,
                               copy_images = FALSE,
                               ...) {
  args <- c()

  # smart quotes, etc.
  if (smart)
    args <- c(args, "--smart")

  # no email obfuscation
  args <- c(args, "--email-obfuscation", "none")

  # self contained document
  if (self_contained) {
    if (copy_images)
      stop("Local image copying is incompatible with self-contained documents.")
    validate_self_contained(mathjax)
    args <- c(args, "--self-contained")
  }

  # custom args
  args <- c(args, pandoc_args)

  preserved_chunks <- character()

  pre_processor <- function (metadata, input_file, runtime, knit_meta,
                             files_dir) {

    args <- c()

    # use files_dir as lib_dir if not explicitly specified
    if (is.null(lib_dir))
      lib_dir <<- files_dir

    # handle theme
    if (!is.null(theme)) {
      theme <- match.arg(theme, themes())
      if (identical(theme, "default"))
        theme <- "bootstrap"
      args <- c(args, "--variable", paste("theme:", theme, sep=""))
    }

    # resolve and inject extras
    if (!is.null(theme))
      format_deps <- list(html_dependency_jquery(),
                          html_dependency_bootstrap(theme))
    else
      format_deps <- NULL
    extras <- html_extras_for_document(knit_meta, runtime, dependency_resolver,
                                       format_deps)
    args <- c(args, pandoc_html_extras_args(extras, self_contained, lib_dir))

    # mathjax
    args <- c(args, pandoc_mathjax_args(mathjax,
                                        template,
                                        self_contained,
                                        lib_dir))

    input_str <- readLines(input_file, warn = FALSE, encoding = "bytes")
    preserve <- extract_preserve_chunks(input_str)
    if (!identical(preserve$value, input_str))
      writeLines(preserve$value, input_file, useBytes = TRUE)
    preserved_chunks <<- preserve$chunks

    args
  }

  post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    if (length(preserved_chunks) == 0 && !isTRUE(copy_images))
      return(output_file)

    output_str <- readLines(output_file, warn = FALSE, encoding = "bytes")

    if (length(preserved_chunks) > 0)
      output_str <- restore_preserve_chunks(output_str, preserved_chunks)

    # if requested, move supporting images to library folder, and rewrite
    # references
    if (copy_images) {
      image_copier <- function(img_src, src) {
        in_file <- utils::URLdecode(src)
        if (length(in_file) && file.exists(in_file)) {
          # create a unique image name in the library folder and copy the
          # image there
          target_img_file <- file.path(lib_dir, createUniqueId(16))
          target_img_file <- paste(target_img_file, tools::file_ext(in_file),
                                   sep = ".")
          file.copy(in_file, target_img_file)
          # replace the reference in the document
          img_src <- sub(src, target_img_file, img_src)
        }
        img_src
      }
      output_str <- process_images(output_str, image_copier)
    }

    writeLines(output_str, output_file, useBytes = TRUE)
    output_file
  }

  output_format(
    knitr = NULL,
    pandoc = pandoc_options(to = "html", from = NULL, args = args),
    clean_supporting = FALSE,
    pre_processor = pre_processor,
    post_processor = post_processor
  )
}

# extract_preserve_chunks and restore_preserve_chunks comprise a mechanism for
# ensuring that arbitrary regions of HTML are protected from any transformation
# by pandoc. This is achieved by replacing such regions with a benign but unique
# string (like a GUID) before pandoc processing, and restoring those regions
# after pandoc processing.


# extract_preserve_chunks looks for regions in strval marked by
# <!--html_preserve-->...<!--/html_preserve--> and replaces each such region
# with a long unique ID. The return value is a list with $value as the string
# with the regions replaced, and $chunks as a named character vector where the
# names are the IDs and the values are the regions that were extracted.
#
# Nested regions are handled appropriately; the outermost region is what's used
# and any inner regions simply have their boundaries removed before the values
# are stashed in $chunks.
extract_preserve_chunks <- function(strval) {

  # Literal start/end marker text. Case sensitive.
  startmarker <- "<!--html_preserve-->"
  endmarker <- "<!--/html_preserve-->"
  # Start and end marker length MUST be different, it's how we tell them apart
  startmarker_len <- nchar(startmarker)
  endmarker_len <- nchar(endmarker)
  # Pattern must match both start and end markers
  pattern <- "<!--/?html_preserve-->"

  # It simplifies string handling greatly to collapse multiple char elements
  if (length(strval) != 1)
    strval <- paste(strval, collapse = "\n")

  # matches contains the index of all the start and end markers
  matches <- gregexpr(pattern, strval, useBytes = TRUE)[[1]]
  lengths <- attr(matches, "match.length")

  # No markers? Just return.
  if (matches[[1]] == -1)
    return(list(value = strval, chunks = character(0)))

  # If TRUE, it's a start; if FALSE, it's an end
  boundary_type <- lengths == startmarker_len

  # Positive number means we're inside a region, zero means we just exited to
  # the top-level, negative number means error (an end without matching start).
  # For example:
  # boundary_type - TRUE TRUE FALSE FALSE TRUE FALSE
  # preserve_level - 1 2 1 0 1 0
  preserve_level <- cumsum(ifelse(boundary_type, 1, -1))

  # Sanity check.
  if (any(preserve_level < 0) || tail(preserve_level, 1) != 0) {
    stop("Invalid nesting of html_preserve directives")
  }

  # Identify all the top-level boundary markers. We want to find all of the
  # elements of preserve_level whose value is 0 and preceding value is 1, or
  # whose value is 1 and preceding value is 0. Since we know that preserve_level
  # values can only go up or down by 1, we can simply shift preserve_level by
  # one element and add it to preserve_level; in the result, any value of 1 is a
  # match.
  is_top_level <- 1 == (preserve_level + c(0, preserve_level[-length(preserve_level)]))

  preserved <- character(0)

  top_level_matches <- matches[is_top_level]
  # Iterate backwards so string mutation doesn't screw up positions for future
  # iterations
  for (i in seq.int(length(top_level_matches) - 1, 1, by = -2)) {
    start_outer <- top_level_matches[[i]]
    start_inner <- start_outer + startmarker_len
    end_inner <- top_level_matches[[i+1]]
    end_outer <- end_inner + endmarker_len

    id <- createUniqueId(16)
    preserved[id] <- gsub(pattern, "",
      substr(strval, start_inner, end_inner-1), useBytes = TRUE)

    strval <- paste(
      substr(strval, 1, start_outer - 1),
      id,
      substr(strval, end_outer, nchar(strval)),
      sep="")
    substr(strval, start_outer, end_outer-1) <- id
  }

  list(value = strval, chunks = preserved)
}

restore_preserve_chunks <- function(strval, chunks) {
  for (id in names(chunks))
    strval <- gsub(id, chunks[[id]], strval, fixed = TRUE, useBytes = TRUE)
  strval
}
