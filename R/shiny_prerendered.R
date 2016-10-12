
prerendered_shiny_app <- function(input_rmd, encoding, render_args) {

  # get rendered html
  html <- prerendered_shiny_html(input_rmd, encoding, render_args)

  # create the server environment
  server_envir = new.env(parent = globalenv())

  # extract the server-start context
  html_lines <- strsplit(html, "\\r?\\n")[[1]]
  server_start_context <- extract_prerendered_context(html_lines, "server_start")
  onStart <- function() {
    eval(parse(text = server_start_context), envir = server_envir)
  }

  # extract the server context
  server_context <- extract_prerendered_context(html_lines, "server")
  server_envir$server_context <- server_context
  server <- function(input, output, session) {
    eval(parse(text = server_context))
  }
  environment(server) <- server_envir

  # create shiny app
  shiny::shinyApp(
    ui = html,
    server = server,
    onStart = onStart,
    uiPattern = "^/$|^(/.*\\.[Rr][Mm][Dd])$"
  )
}


extract_prerendered_context <- function(html_lines, context) {

  # look for lines that start the context
  pattern <- paste0('<script type="application/shiny-prerendered" data-context="', context, '">')
  matches <- regmatches(html_lines, regexec(pattern, html_lines))

  # extract the code within the contexts
  in_context <- FALSE
  context_lines <- character()
  for (i in 1:length(matches)) {
    if (length(matches[[i]]) > 0) {
      in_context <- TRUE
      next
    }
    else if (in_context && identical(html_lines[[i]], "</script>")) {
      in_context <- FALSE
    }
    if (in_context)
      context_lines <- c(context_lines, html_lines[[i]])
  }
  context_lines
}

prerendered_shiny_html <- function(input_rmd, encoding, render_args) {

  # resolve input html file and directory (may include a render)
  rendered_html <- prerender(input_rmd, encoding, render_args)
  output_dir <- dirname(rendered_html)

  # add some resource paths
  add_resource_path <- function(path) {
    if (utils::file_test("-d", path))
      shiny::addResourcePath(basename(path), path)
  }
  files_dir <- knitr_files_dir(rendered_html)
  add_resource_path(files_dir)
  add_resource_path(file.path(output_dir,"css"))
  add_resource_path(file.path(output_dir,"js"))
  add_resource_path(file.path(output_dir,"images"))
  add_resource_path(file.path(output_dir,"www"))

  # generate html w/ dependencies
  deps_path <- file.path(files_dir, "shiny.dep")
  dependencies <- list()
  if (file.exists(deps_path)) {
    read_deps <- base::file(deps_path, open = "rb")
    on.exit(close(read_deps), add = TRUE)
    dependencies <- unserialize(read_deps)
  }
  # return it
  shinyHTML_with_deps(rendered_html, dependencies)
}


prerender <- function(input_rmd, encoding, render_args) {

  # determine the path to the rendered_html
  output_file <- render_args$output_file
  if (is.null(output_file))
    output_file <- file_with_ext(basename(input_rmd), "html")
  output_dir <- render_args$output_dir
  if (is.null(output_dir))
    output_dir <- dirname(input_rmd)
  rendered_html <- file.path(output_dir, output_file)

  # determine whether we need to render the Rmd in advance
  prerender_option <- tolower(Sys.getenv("RMARKDOWN_RUN_PRERENDER", "1"))

  if (file.access(output_dir, 2) != 0) {
    prerender <- FALSE
  }
  else if (identical(prerender_option, "0")) {
    prerender <- FALSE
  }
  else if (identical(prerender_option, "1")) {

    # determine the last modified time of the output file
    if (file.exists(rendered_html))
      output_last_modified <- as.integer(file.info(rendered_html)$mtime)
    else
      output_last_modified <- 0L

    # short circuit for Rmd modified. if it hasn't been modified since the
    # html was generated look at external resources
    input_last_modified <- as.integer(file.info(input_rmd)$mtime)
    if (input_last_modified > output_last_modified) {
      prerender <- TRUE
    }
    else {
      # find external resources referenced by the file
      external_resources <- find_external_resources(input_rmd, encoding)

      # get paths to external resources
      input_files <- c(input_rmd,
                       file.path(output_dir, external_resources$path))

      # what's the maximum last_modified time of an input file
      input_last_modified <- max(as.integer(file.info(input_files)$mtime),
                                 na.rm = TRUE)

      # render if an input file was modified after the output file
      prerender <- input_last_modified > output_last_modified
    }
  }
  else {
    stop("Invalid value '", prerender_option, "' for RMARKDOWN_RUN_PRERENDER")
  }

  # prerender if necessary
  if (prerender) {

    # execute the render
    args <- merge_lists(list(input = input_rmd,
                             encoding = encoding,
                             envir = new.env()),
                        render_args)
    rendered_html <- do.call(render, args)
  }

  # normalize path and return it
  normalizePath(rendered_html, winslash = "/")
}

