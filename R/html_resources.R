
# given HTML input and a callback, invokes the callback on everything in the 
# HTML that looks like it might point to a resource.
call_resource_attrs <- function(html, callback) {
  
  attr_handler <- function(node, attr_name) {
    # check to see if the node has a value for the given attribute
    val <- node$attributes[attr_name]
    if (is.null(val) || is.na(val)) {
      # no value, continue
      node
    } else
      # value found, invoke callbcak
      callback(node, attr_name, val)
  }
  
  # parse the HTML and invoke handlers on all elements that look like they might
  # refer to external resources
  XML::htmlTreeParse(file = html, asText = TRUE, handlers = list(
      img    = function(node) { attr_handler(node, "src")  },
      link   = function(node) { attr_handler(node, "href") },
      iframe = function(node) { attr_handler(node, "src")  },
      object = function(node) { attr_handler(node, "data") },
      script = function(node) { attr_handler(node, "src")  },
      audio  = function(node) { attr_handler(node, "src")  },
      video  = function(node) { attr_handler(node, "src")  },
      embed  = function(node) { attr_handler(node, "src")  }
    ))
  
  invisible(NULL)
}

find_resources <- function(rmd_file) {
  # assert that this is rmarkdown 
  if (!identical(tolower(tools::file_ext(rmd_file)), "rmd")) {
    stop("Resource discovery is only supported for R Markdown files.")
  }
  
  # copy to a temporary folder
  md_file <- tempfile(fileext = "md")
  file.copy(rmd_file, md_file)
  on.exit(unlink(md_file), add = TRUE)
  
  # render "raw" markdown to HTML
  html_file <- tempfile(fileext = "html")
  render(input = md_file, output_file = html_file, 
         output_options = list(self_contained = FALSE))
  on.exit(unlink(html_file), add = TRUE)
  
  # resource accumulator
  discovered_resources <- c()
  discover_resource <- function(node, att, val) {
    res_file <- utils::URLdecode(val)
    if (file.exists(res_file)) {
      discovered_resources <<- c(discovered_resources, res_file)
    }
    node
  }
 
  # find the resources
  call_resource_attrs(readLines(html_file, warn = FALSE, encoding = "UTF-8"),
    discover_resource)
  
  # TODO: look for paths in R chunks
  discovered_resources 
}
