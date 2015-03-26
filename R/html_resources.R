#' Find External Resource References
#'
#' Given an R Markdown document or HTML file, attempt to determine the set of
#' additional files needed in order to render and display the document.
#' 
#' @param input_file path to the R Markdown document or HTML file to process
#' @param encoding the encoding of the document
#' 
#' @details This routine applies heuristics in order to scan a document for
#'   possible resource references. 
#'   
#'   In R Markdown documents, it looks for references to files implicitly
#'   referenced in Markdown (e.g. \code{![alt](img.png)}), in the document's
#'   YAML header, in raw HTML chunks, and as quoted strings in R code chunks
#'   (e.g. \code{read.csv("data.csv")}).
#'   
#'   Resources specified explicitly in the YAML header for R Markdown documents
#'   are also returned. To specify resources in YAML, use the
#'   \code{resource_files} key:
#'   
#'   \preformatted{---
#'title: My Document
#'author: My Name
#'resource_files:
#'  - data/mydata.csv
#'  - images/figure.png
#'---}
#'
#'   Each item in the \code{resource_files} list can refer to: 
#'   \enumerate{
#'   \item A single file, such as \code{images/figure.png}, or
#'   \item A directory, such as \code{resources/data}, in which case all of the
#'     directory's content will be recursively included, or
#'   \item A wildcard pattern, such as \code{data/*.csv}, in which case all of
#'     the files matching the pattern will be included. No recursion is done in
#'     this case.
#'   }
#'   
#'   In HTML files (and raw HTML chunks in R Markdown documents), this routine
#'   searches for resources specified in common tag attributes, such as
#'   \code{<img src="...">}, \code{<link href="...">}, etc.
#'   
#'   In all cases, only resources that exist on disk and are contained in the
#'   document's directory (or a child thereof) are returned.
#'   
#' @return A data frame with the following columns:
#'   \describe{
#'    \item{path}{The relative path from the document to the resource}
#'    \item{explicit}{Whether the resource was specified explicitly 
#'      (\code{TRUE}) or discovered implicitly (\code{FALSE})}
#'    \item{web}{Whether the resource is needed to display a Web page rendered
#'      from the document}
#'   }
#'     
#' @export
find_external_resources <- function(input_file, 
                                    encoding = getOption("encoding")) {
  # ensure we're working with valid input
  ext <- tolower(tools::file_ext(input_file))
  if (!(ext %in% c("rmd", "html"))) {
    stop("Resource discovery is only supported for R Markdown files or HTML ",
         "files.")
  }
  if (!file.exists(input_file)) {
    stop("The input file file '", input_file, "' does not exist.")
  }
  
  # set up the frame we'll use to report results
  discovered_resources <- data.frame(
    path = character(0),
    explicit = logical(0),
    web = logical(0))
  input_dir <- dirname(normalizePath(input_file, winslash = "/"))
  
  # discover a single resource--tests a string to see if it corresponds to a 
  # resource on disk; if so, adds it to the list of known resources and returns
  # TRUE
  discover_single_resource <- function(path, explicit, web) {
    if (is.character(path) && 
        length(path) == 1 && 
        file.exists(file.path(input_dir, path))) {
      discovered_resources <<- rbind(discovered_resources, data.frame(
        path = path, 
        explicit = explicit, 
        web = web,
        stringsAsFactors = FALSE))
      TRUE
    } else {
      FALSE
    }
  }
  
  # run the main resource discovery appropriate to the file type
  if (ext == "rmd") {
    # discover R Markdown doc resources--scans the document itself as described
    # in comments above, renders as Markdown, and invokes HTML discovery 
    # on the result
    discover_rmd_resources(input_file, encoding, discover_single_resource)
  } else if (ext == "html") {
    # discover HTML resources
    discover_html_resources(input_file, encoding, discover_single_resource)
    
    # if the HTML file represents a rendered R Markdown document, it may have a 
    # sidecar _files folder; include that if it's present
    sidecar_files_dir <- knitr_files_dir(input_file)
    files_dir_info <- file.info(sidecar_files_dir)
    if (isTRUE(files_dir_info$isdir)) {
      # get a list of the files in the folder
      knitr_files <- list.files(path = sidecar_files_dir, 
                                recursive = TRUE, include.dirs = FALSE)
      
      # remove those we already discovered via heuristics
      knitr_files <- file.path(basename(sidecar_files_dir), knitr_files)
      knitr_files <- setdiff(knitr_files, discovered_resources$path)
      
      # add the unique files discovered in the _files folder
      discovered_resources <- rbind(discovered_resources, data.frame(
        path = knitr_files, 
        explicit = FALSE, 
        web = TRUE,
        stringsAsFactors = FALSE))
    }
  }
  
  # clean row names (they're not meaningful)
  rownames(discovered_resources) <- NULL
  
  # convert paths from factors if necssary, and clean any redundant ./ leaders
  discovered_resources$path <- as.character(discovered_resources$path)
  has_prefix <- grepl("^\\./", discovered_resources$path) 
  discovered_resources$path[has_prefix] <- substring(
    discovered_resources$path[has_prefix], 3)
    
  discovered_resources 
}

# discovers resources in a single HTML file with the given encoding
discover_html_resources <- function(html_file, encoding, 
                                    discover_single_resource) {
  # resource accumulator
  discover_resource <- function(node, att, val, idx) {
    res_file <- utils::URLdecode(val)
    discover_single_resource(res_file, FALSE, TRUE)
  }
  
  # create a single string with all of the lines in the document
  html_lines <- paste(
      readLines(html_file, warn = FALSE, encoding = encoding), collapse = "\n")
  
  # if the lines aren't encoded in UTF-8, re-encode them to UTF-8; this is 
  # necessary since we presume the encoding when parsing the HTML
  if (encoding != "UTF-8") {
    html_lines <- enc2utf8(html_lines)
  }
  
  # parse the HTML and invoke our resource discovery callbacks
  call_resource_attrs(html_lines, discover_resource)
}

# discovers resources in a single R Markdown document
discover_rmd_resources <- function(rmd_file, encoding, 
                                   discover_single_resource) {
  # create a UTF-8 encoded Markdown file to serve as the resource discovery 
  # source
  md_file <- tempfile(fileext = ".md")
  output_dir <- dirname(md_file)
  rmd_content <- read_lines_utf8(rmd_file, encoding)
  writeLines(rmd_content, md_file, useBytes = TRUE)
  
  # create a vector of temporary files; anything in here will be cleaned up on
  # exit 
  temp_files <- md_file
  on.exit(unlink(temp_files, recursive = TRUE), add = TRUE)
  
  # discovers render-time resources; if any are found, adds them to the list of
  # discovered resources and copies them alongside the input document.
  discover_render_resource <- function(output_render_file) {
    if (discover_single_resource(output_render_file, FALSE, FALSE)) {
      # mirror original directory structure so we don't need to mutate input
      # prior to render
      output_target_file <- file.path(output_dir, output_render_file)
      if (!file.exists(dirname(output_target_file))) {
        dir.create(dirname(output_target_file), showWarnings = FALSE,
                   recursive = TRUE)
      }
      
      # copy the original resource to the temporary render folder
      file.copy(file.path(input_dir, output_render_file),
                output_target_file)
      
      # clean up this file when we're done
      temp_files <<- c(temp_files, output_target_file)
    }
  }
  
  # parse the YAML front matter to discover resources named there
  front_matter <- parse_yaml_front_matter(
    readLines(md_file, warn = FALSE, encoding = "UTF-8"))
  
  # Check for content referred to by output format calls to the includes 
  # function (for generating headers/footers/etc. at render time), and for 
  # references to files in pandoc arguments. 
  # 
  # These will be needed to produce even a vanilla Markdown variant of the input
  # document, so copy them to the temporary folder in preparation for rendering
  # (in addition to marking them as required resources).
  output_formats <- front_matter$output
  if (is.list(output_formats)) {
    for (output_format in output_formats) {
      output_render_files <- c(output_format$includes, 
                               output_format$pandoc_args)
      for (output_render_file in output_render_files) {
        discover_render_resource(output_render_file)
      }
    }
  }
  
  # check for explicitly named resources
  if (!is.null(front_matter$resource_files)) {
    resources <- lapply(front_matter$resource_files, function(res) {
      explicit_res <- if (is.character(res)) {
        list(path = res, explicit = TRUE, web = is_web_file(res))
      } else if (is.list(res) && length(names(res)) > 0) {
        # list--happens when web flag is specified explicitly in YAML.
        list(path = names(res)[[1]], 
             explicit = TRUE,
             web = if (is.null(res$web)) is_web_file(res) else res$web)
      } else  {
        # no idea what this is, skip it
        NULL
      }
      
      # check the extracted filename to see if it exists
      if (!is.null(explicit_res)) {
        if (grepl("*", explicit_res$path, fixed = TRUE)) {
          # if the resource file spec includes a wildcard, list the files 
          # that match the pattern
          files <- list.files(
            path = file.path(input_dir, dirname(explicit_res$path)), 
            pattern = glob2rx(basename(explicit_res$path)),
            recursive = FALSE,
            include.dirs = FALSE)
          lapply(files, function(f) {
            list(path = file.path(dirname(explicit_res$path), f), 
                 explicit = TRUE,
                 web = is_web_file(f)) })
        } else {
          # no wildcard, see whether this resource refers to a directory or to
          # an individual file
          info <- file.info(file.path(input_dir, explicit_res$path))
          if (is.na(info$isdir)) {
            # implies that the file doesn't exist (should we warn here?)
            NULL
          } else if (isTRUE(info$isdir)) {
            # if the resource file spec is a directory, include all the files in
            # the directory, recursively
            files <- list.files(
              path = file.path(input_dir, explicit_res$path),
              recursive = TRUE,
              include.dirs = FALSE)
            lapply(files, function(f) {
              list(path = file.path(explicit_res$path, f), 
                   explicit = TRUE,
                   web = is_web_file(f)) })
          } else {
            # isdir is false--this is an individual file; return it
            list(explicit_res)
          }
        }
      } else {
        list(explicit_res)
      }
    })
    discovered_resources <- do.call(rbind.data.frame, do.call(c, resources))
  }
  
  # check for knitr child documents
  chunk_lines <- gregexpr(knitr::all_patterns$md$chunk.begin, rmd_content,
                          perl = TRUE) 
  for (idx in seq_along(chunk_lines)) {
    chunk_line <- chunk_lines[idx][[1]]
    if (chunk_line < 0)
      next
    chunk_start <- attr(chunk_line, "capture.start", exact = TRUE) + 1
    chunk_text <- substr(rmd_content[idx], chunk_start,
                         chunk_start + attr(chunk_line, "capture.length", 
                                            exact = TRUE) - 2)
    for (child_expr in c("\\bchild\\s*=\\s*'([^']+)'", 
                         "\\bchild\\s*=\\s*\"([^\"]+)\"")) {
      child_match <- gregexpr(child_expr, chunk_text, perl = TRUE)[[1]]
      if (child_match > 0) {
        child_start <- attr(child_match, "capture.start", exact = TRUE)
        child_text <- substr(chunk_text, child_start, 
                             child_start + attr(child_match, "capture.length", 
                                                exact = TRUE) - 1)
        discover_render_resource(child_text)
      }
    }
  }
 
  # render "raw" markdown to HTML
  html_file <- tempfile(fileext = ".html")
  render(input = md_file, output_file = html_file, 
         output_options = list(self_contained = FALSE), quiet = TRUE,
         encoding = "UTF-8")
  
  # clean up output file and its supporting files directory
  temp_files <- c(temp_files, html_file, knitr_files_dir(md_file))
  
  # run the HTML resource discovery mechanism on the rendered output
  discover_html_resources(html_file, "UTF-8", discover_single_resource)  
  
  # purl the file to extract just the R code 
  r_file <- tempfile(fileext = ".R")
  knitr::purl(md_file, output = r_file, quiet = TRUE, documentation = 0,
              encoding = "UTF-8")
  temp_files <- c(temp_files, r_file)
  r_lines <- readLines(r_file, warn = FALSE, encoding = "UTF-8") 
  
  # clean comments from the R code (simply; consider: # inside strings)
  r_lines <- sub("#.*$", "", r_lines)
    
  # find quoted strings in the code and attempt to ascertain whether they are
  # files on disk
  r_lines <- paste(r_lines, collapse = "\n")
  quoted_strs <- Reduce(c, lapply(c("\"[^\"]+\"", "'[^']+'"), function(pat) {
    matches <- unlist(regmatches(r_lines, gregexpr(pat, r_lines)))
    substr(matches, 2, nchar(matches) - 1)
  }))
  
  # consider any quoted string containing a valid relative path to a file that 
  # exists on disk to be a reference
  for (quoted_str in quoted_strs) {
    if (file.exists(file.path(input_dir, quoted_str))) {
      discovered_resources <- rbind(discovered_resources, data.frame(
        path = quoted_str, 
        explicit = FALSE,
        web = FALSE,
        stringsAsFactors = FALSE))
    }
  }
}

# given a filename, return true if the file appears to be a web file
is_web_file <- function(filename) {
  tolower(tools::file_ext(filename)) %in% c(
    "css",
    "gif",
    "htm",
    "html",
    "jpeg",
    "jpg",
    "js",
    "mp3",
    "mp4",
    "png",
    "wav")
}

