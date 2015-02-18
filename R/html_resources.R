#' Find External Resource References
#'
#' Given an R Markdown document, attempt to determine the set of additional
#' files needed in order to render and display the document.
#' 
#' @param rmd_file path to the R Markdown document to process
#' @param encoding the encoding of the R Markdown document
#' 
#' @details This routine applies heuristics in order to scan a document for
#'   possible resource references. 
#'   
#'   It looks for references to files implicitly referenced in Markdown (e.g.
#'   \code{![alt](img.png)}), in the document's YAML header, in raw HTML chunks,
#'   and as quoted strings in R code chunks (e.g. \code{read.csv("data.csv")}).
#'   
#'   Resources specified explicitly in the document's YAML header are also 
#'   returned. To specify resources in YAML, use the \code{resource_files} key:
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
#'   Only resources that exist on disk and are contained in the document's
#'   directory (or a child thereof) are returned.
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
find_external_resources <- function(rmd_file, 
                                    encoding = getOption("encoding")) {
  # ensure we're working with valid input
  if (!identical(tolower(tools::file_ext(rmd_file)), "rmd")) {
    stop("Resource discovery is only supported for R Markdown files.")
  }
  if (!file.exists(rmd_file)) {
    stop("The input file file '", rmd_file, "' does not exist.")
  }
  
  # set up the frame we'll use to report results
  discovered_resources <- data.frame(
    path = character(0),
    explicit = logical(0),
    web = logical(0))
  input_dir <- dirname(normalizePath(rmd_file, winslash = "/"))
  
  # create a UTF-8 encoded Markdown file to serve as the resource discovery 
  # source
  md_file <- tempfile(fileext = ".md")
  on.exit(unlink(md_file), add = TRUE)
  rmd_content <- read_lines_utf8(rmd_file, encoding)
  writeLines(rmd_content, md_file, useBytes = TRUE)
  
  # parse the YAML front matter to discover explicit resources
  front_matter <- parse_yaml_front_matter(
    readLines(md_file, warn = FALSE, encoding = "UTF-8"))
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
 
  # render "raw" markdown to HTML
  html_file <- tempfile(fileext = ".html")
  render(input = md_file, output_file = html_file, 
         output_options = list(self_contained = FALSE), quiet = TRUE,
         encoding = "UTF-8")
  on.exit(unlink(html_file), add = TRUE)
  
  # resource accumulator
  discover_resource <- function(node, att, val, idx) {
    res_file <- utils::URLdecode(val)
    if (file.exists(file.path(input_dir, res_file))) {
      discovered_resources <<- rbind(discovered_resources, data.frame(
        path = val, 
        explicit = FALSE, 
        web = TRUE))
    }
  }
 
  # parse the HTML and invoke our resource discovery callbacks
  call_resource_attrs(readLines(html_file, warn = FALSE, encoding = "UTF-8"),
    discover_resource)
  
  # purl the file to extract just the R code 
  r_file <- tempfile(fileext = ".R")
  knitr::purl(md_file, output = r_file, quiet = TRUE, documentation = 0,
              encoding = "UTF-8")
  on.exit(unlink(r_file), add = TRUE)
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
        web = FALSE))
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

# given HTML input and a callback, invokes the callback on everything in the 
# HTML that looks like it might point to a resource.
call_resource_attrs <- function(html, callback) {
  
  attr_handler <- function(tag, attr_name, attr_value, attr_idx) {
    if ((tag == "img"    && attr_name == "src")  ||
        (tag == "link"   && attr_name == "href") ||
        (tag == "object" && attr_name == "data") || 
        (tag == "script" && attr_name == "src")  ||
        (tag == "audio"  && attr_name == "src")  ||
        (tag == "video"  && attr_name == "src")  ||
        (tag == "embed"  && attr_name == "src"))
    {
      # value found, invoke callbcak
      callback(tag, attr_name, attr_value, attr_idx)
    }
  }
  
  # parse the HTML and invoke handlers on all elements that look like they might
  # refer to external resources
  html_extract_values(html, attr_handler)
  
  invisible(NULL)
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
    "png")
}

