#' Run a Shiny document
#'
#' Start a Shiny server for the given document, and render it for display.
#'
#' The \code{run} function runs a Shiny document by starting a Shiny
#' server associated with the document. The \code{shiny_args} parameter can be
#' used to configure the server; see the \code{\link[shiny:runApp]{runApp}}
#' documentation for details.
#'
#' Once the server is started, the document will be rendered using
#' \code{\link{render}}. The server will initiate a render of the document
#' whenever necessary, so it is not necessary to call \code{run} every time
#' the document changes: if \code{auto_reload} is \code{TRUE}, saving the
#' document will trigger a render. You can also manually trigger a render by
#' reloading the document in a Web browser.
#'
#' The server will render any R Markdown (\code{.Rmd}) document in \code{dir};
#' the \code{file} argument specifies only the initial document to be
#' rendered and viewed. You can therefore link to other documents in the
#' directory using standard Markdown syntax, e.g.
#' \code{[Analysis Page 2](page2.Rmd)}.
#'
#' If \code{default_file} is not specified, nor is a file specified on the
#' URL, then the default document to serve at \code{/} is chosen from (in
#' order of preference):
#' \itemize{
#'   \item{If \code{dir} contains only one \code{Rmd}, that \code{Rmd}.}
#'   \item{The file \file{index.Rmd}, if it exists in \code{dir}.}
#'   \item{The first \code{Rmd} that has \code{runtime: shiny} in its YAML metadata.}
#'   \item{The file \file{index.html} (or \file{index.htm}), if it exists in \code{dir}.}
#' }
#'
#' If you wish to share R code between your documents, place it in a file
#' named \code{global.R} in \code{dir}; it will be sourced into the global
#' environment.
#' @param file Path to the R Markdown document to launch in a web browser.
#'   Defaults to \code{index.Rmd} in the current working directory, but may be
#'   \code{NULL} to skip launching a browser.
#' @param dir The directory from which to to read input documents. Defaults to
#'   the parent directory of \code{file}.
#' @param default_file The file to serve at the Shiny server's root URL. If
#'   \code{NULL} (the default), a sensible default is chosen (see Details)
#' @param auto_reload If \code{TRUE} (the default), automatically reload the
#'   Shiny application when the file currently being viewed is changed on disk.
#' @param shiny_args Additional arguments to \code{\link[shiny:runApp]{runApp}}.
#' @param render_args Additional arguments to \code{\link{render}}.
#' @return Invisible NULL.
#' @note Unlike \code{\link{render}}, \code{run} does not render the document to
#'   a file on disk. In most cases a Web browser will be started automatically
#'   to view the document; see \code{launch.browser} in the
#'   \code{\link[shiny:runApp]{runApp}} documentation for details.
#'
#'   When using an external web browser with the server, specify the name of the
#'   R Markdown file to view in the URL (e.g.
#'   \code{http://127.0.0.1:1234/foo.Rmd}). A URL without a filename will show
#'   the \code{default_file} as described above.
#' @examples
#' \dontrun{
#' # Run the Shiny document "index.Rmd" in the current directory
#' rmarkdown::run()
#'
#' # Run the Shiny document "shiny_doc.Rmd" on port 8241
#' rmarkdown::run("shiny_doc.Rmd", shiny_args = list(port = 8241))
#' }
#' @export
run <- function(file = "index.Rmd", dir = dirname(file), default_file = NULL,
                auto_reload = TRUE, shiny_args = NULL, render_args = NULL) {

  # if file is missing and there is an index.qmd (and no index.Rmd) then use that
  if (missing(file) && !file.exists("index.Rmd") && file.exists("index.qmd"))
    file <- "index.qmd"

  # if the file argument is missing then substitute ui.Rmd if it exists
  # (and index.Rmd does not exist)
  if (missing(file) && missing(default_file)) {
    if (!any(file.exists(file.path(dir, paste0("index.", c("qmd", "Rmd"))))) &&
        any(file.exists(file.path(dir, paste0("ui.", c("qmd", "Rmd")))))) {
      uiRmd = file.path(dir, "ui.Rmd")
      file <- ifelse(file.exists(uiRmd), uiRmd, file.path(dir, "ui.qmd"))
    }
  }

  # select the document to serve at the root URL if not user-specified. We exclude
  # documents which start with a leading underscore (same pattern is used to
  # designate "sub-documents" in R Markdown websites and bookdown)
  if (is.null(default_file)) {
    allRmds <- list.files(path = dir, pattern = "^[^_].*\\.[Rrq][Mm][Dd]$")
    if (length(allRmds) == 1) {
      # just one R Markdown document
      default_file <- allRmds
    } else {
      # more than one: look for an index or ui
      index <- which(tolower(allRmds) %in% c("index.rmd", "index.qmd", "ui.rmd", "ui.qmd"))
      if (length(index) > 0) {
        default_file <- allRmds[index[1]]
      } else {
        # look for first one that has runtime: shiny
        for (rmd in allRmds) {
          yaml <- yaml_front_matter(file.path(dir, rmd))
          if (is_shiny(yaml$runtime, yaml$server)) {
            default_file <- rmd
            break
          }
        }
      }
    }
  }

  if (is.null(default_file)) {
    # no R Markdown default found; how about an HTML?
    indexHtml <- list.files(dir, "(index|ui).html?", ignore.case = TRUE)
    if (length(indexHtml) > 0) default_file <- indexHtml[1]
  }

  # form and test locations
  dir <- normalize_path(dir)
  if (!dir_exists(dir)) stop("The directory '", dir, "' does not exist")

  if (!is.null(file)) {
    # compute file path relative to directory (remove common directory prefix
    # if it exists)
    file_rel <- normalize_path(file)
    if (identical(substr(file_rel, 1, nchar(dir)), dir))
      file_rel <- substr(file_rel, nchar(dir) + 2, nchar(file_rel))

    # if we don't have a default to launch, make sure the user-specified file
    # exists
    if (is.null(default_file)) {
      resolved <- resolve_relative(dir, file_rel)
      if (is.null(resolved) || !file.exists(resolved))
        stop("The file '", file, "' does not exist in the directory '", dir, "'")
    }
  }

  if (is.null(render_args$envir)) render_args$envir <- parent.frame()

  # determine the runtime of the target file
  target_file <- file %||% file.path(dir, default_file)
  yaml_front <- if (length(target_file)) yaml_front_matter(target_file)
  runtime <- yaml_front$runtime
  server <- yaml_front$server
  theme <- render_args$output_options$theme
  # Let shiny::getCurrentTheme() know about the yaml's theme, so
  # things like `bslib::bs_themer()` can work with prerendered documents.
  # Also note that we add the actual shiny::bootstrapLib() dependency
  # inside the document's pre-processing hook so the 'last' version of
  # the theme wins out
  if (length(target_file)) {
    format <- output_format_from_yaml_front_matter(read_utf8(target_file))
    old_theme <- shiny::getCurrentTheme()
    on.exit(set_current_theme(old_theme), add = TRUE)
    set_current_theme(resolve_theme(format$options$theme))
  }

  # run using the requested mode
  if (is_shiny_prerendered(runtime, server)) {

    # get the pre-rendered shiny app
    app <- shiny_prerendered_app(target_file, render_args = render_args)
  } else {

    # add rmd_resources handler on start
    onStart <- function() {
      global_r <- file.path.ci(dir, "global.R")
      if (file.exists(global_r)) {
        source(global_r, local = FALSE)
      }
      shiny::addResourcePath("rmd_resources", pkg_file("rmd/h/rmarkdown"))
    }

    # combine the user-supplied list of Shiny arguments with our own and start
    # the Shiny server; handle requests for the root (/) and any R markdown files
    # within
    app <- shiny::shinyApp(ui = rmarkdown_shiny_ui(dir, default_file),
                           uiPattern = "^/$|^/index\\.html?$|^(/.*\\.[Rrq][Mm][Dd])$",
                           onStart = onStart,
                           server = rmarkdown_shiny_server(
                             dir, default_file, auto_reload, render_args))

    # cleanup evaluated cache when the current shiny app exits
    on.exit({
      .globals$evaluated_global_chunks <- character()
    }, add = TRUE)
  }

  # launch the app and open a browser to the requested page, if one was
  # specified
  launch_browser <- shiny_args$launch.browser %||% (!is.null(file) && interactive())
  if (isTRUE(launch_browser)) {
    launch_browser <- function(url) {
      url <- paste(url, file_rel, sep = "/")
      browser <- getOption("shiny.launch.browser")
      if (is.function(browser)) {
        browser(url)
      } else {
        utils::browseURL(url)
      }
    }
  }

  shiny_args <- merge_lists(list(appDir = app, launch.browser = launch_browser),
                            shiny_args)
  ret <- do.call(shiny::runApp, shiny_args)
  invisible(ret)
}

# create the Shiny server function
rmarkdown_shiny_server <- function(dir, file, auto_reload, render_args) {
  function(input, output, session) {
    path_info <- utils::URLdecode(session$request$PATH_INFO)
    # strip /websocket/ from the end of the request path if present
    path_info <- sub("/websocket/$", "", path_info)
    if (path_info == "") path_info <- file

    file <- resolve_relative(dir, path_info)
    reactive_file <- if (auto_reload)
      shiny::reactiveFileReader(500, session, file, identity)
    else
      function() { file }

    envir_global <- render_args[['envir']]
    envir_server <- list2env(list(
      input = input, output = output, session = session
    ), parent = envir_global)
    render_args$envir <- new.env(parent = envir_server)

    # when the file loads (or is changed), render to a temporary file, and
    # read the contents into a reactive value
    doc <- shiny::reactive({
      # check to see whether we have cached output for this file
      out <- rmd_cached_output(file)
      output_dest <- out$dest

      # if output is cached, return it directly
      if (out$cached) {
        if (nchar(out$resource_folder) > 0) {
          shiny::addResourcePath(basename(out$resource_folder),
                                 out$resource_folder)
        }
        return(out$shiny_html)
      }

      # ensure destination directory exists
      if (!file.exists(dirname(output_dest))) {
        dir.create(dirname(output_dest), recursive = TRUE, mode = "0700")
      }

      # check to see if the output already exists
      resource_folder <- knitr_files_dir(output_dest)

      # clear out performance timings
      perf_timer_reset_all()

      # use a custom dependency resolver that just accumulates the dependencies
      # (we'll pass these to Shiny in a moment)
      dependencies <- list()
      shiny_dependency_resolver <- function(deps) {
        dependencies <<- deps
        list()
      }

      # ensure that the document is not rendered to one page
      output_opts <- list(
        self_contained = FALSE,
        copy_resources = TRUE,
        dependency_resolver = shiny_dependency_resolver)

      # remove console clutter from any previous renders
      message("\f")

      # merge our inputs with those supplied by the user and invoke render
      args <- merge_lists(list(input = reactive_file(),
                               output_file = output_dest,
                               output_dir = dirname(output_dest),
                               output_options = output_opts,
                               intermediates_dir = dirname(output_dest),
                               runtime = "shiny"),
                          render_args)
      result_path <- shiny::maskReactiveContext(do.call(render, args))

      # ensure the resource folder exists, and map requests to it in Shiny
      if (!dir_exists(resource_folder))
        dir.create(resource_folder, recursive = TRUE)
      shiny::addResourcePath(basename(resource_folder), resource_folder)

      # extra dependency: emit performance information collected during render
      dependencies <- append(dependencies, list(create_performance_dependency(resource_folder)))

      # save the structured dependency information
      write_shiny_deps(resource_folder, dependencies)

      # attach rstudio rsiframe script if we are in rstudio (we do this after
      # persisting the dependencies since this dependency is ephemeral)
      if (nzchar(Sys.getenv("RSTUDIO")))
        dependencies <- append(dependencies, list(html_dependency_rsiframe()))

      # when the session ends, remove the rendered document and any supporting
      # files, if they're not cacheable
      if (!isTRUE(out$cacheable)) {
        shiny::onReactiveDomainEnded(shiny::getDefaultReactiveDomain(), function() {
          unlink(result_path)
          unlink(resource_folder, recursive = TRUE)
        })
      }
      shinyHTML_with_deps(result_path, dependencies)
    })

    doc_ui <- shiny::renderUI({
      doc()
    })

    # For test snapshots. (The snapshotPreprocessOutput function was added
    # in shiny 1.0.4.)
    if (exists("snapshotPreprocessOutput", asNamespace("shiny"))) {
      doc_ui <- shiny::snapshotPreprocessOutput(
        doc_ui,
        function(value) {
          # Since the html data can be very large, just record a hash of it.
          value$html <- sprintf("[html data sha1: %s]",
            digest::digest(value$html, algo = "sha1", serialize = FALSE)
          )

          value
        }
      )
    }

    output$`__reactivedoc__` <- doc_ui
  }
}

# create the Shiny UI function
rmarkdown_shiny_ui <- function(dir, file) {
  function(req) {
    # map requests to / to requests for the default--index.Rmd, or another if
    # specified
    req_path <- utils::URLdecode(req$PATH_INFO)
    if (identical(req_path, "/")) {
      req_path <- file
    }

    # request must be for an R Markdown or HTML document
    ext <- tolower(xfun::file_ext(req_path))
    if (!(ext %in% c("rmd", "qmd", "htm", "html"))) return(NULL)

    # document must exist
    target_file <- resolve_relative(dir, req_path)
    if (is.null(target_file) || !file.exists(target_file)) {
      return(NULL)
    }

    tags$div(
      tags$head(
        tags$script(src = "rmd_resources/rmd_loader.js"),
        tags$link(href = "rmd_resources/rmd_loader.css", rel = "stylesheet")
      ),

      # Shiny shows the outer conditionalPanel as long as the document hasn't
      # loaded; the inner rmd_loader is shown by rmd_loader.js as soon as
      # we've been waiting a certain number of ms
      shiny::conditionalPanel(
        "!output.__reactivedoc__",
        tags$div(
          id = "rmd_loader_wrapper",
          tags$div(id = "rmd_loader", style = "display: none",
                   tags$p("Please wait..."),
                   tags$img(src = "rmd_resources/rmd_loader.gif")))),
      shiny::uiOutput("__reactivedoc__")
    )
  }
}

shinyHTML_with_deps <- function(html_file, deps) {

  # read the html_file
  html <- read_utf8(html_file)

  # if we are running in RStudio and have local MathJax then serve locally
  # (for static Rmds we do this substitution inside RStudio as we serve the
  # html over our port, however these documents are served by Shiny so there
  # is no opportunity to do the substituion)
  if (nzchar(Sys.getenv("RSTUDIO"))) {
    local_mathjax <- Sys.getenv("RMARKDOWN_MATHJAX_PATH")
    if (nzchar(local_mathjax)) {
      html <- gsub(pattern = "https://mathjax.rstudio.com/latest/MathJax.js?",
                   replacement = "mathjax-local/MathJax.js?",
                   x = html,
                   fixed = TRUE,
                   useBytes = TRUE)
      shiny::addResourcePath("mathjax-local", local_mathjax)
    }
  }

  # attach dependencies and return HTML
  htmltools::attachDependencies(HTML(one_string(html)), deps)
}

#given an input file, return a list with values indicating whether the input
#file's Shiny document can be cached and, if so, its cached representation if
#available ' @import utils
rmd_cached_output <- function(input) {
  # init return values
  cacheable <- FALSE
  cached <- FALSE
  shiny_html <- NULL
  resource_folder <- ""

  # if the file is raw HTML, return it directly
  if (tolower(xfun::file_ext(input)) %in% c("htm", "html")) {
    return(list(
      cacheable = TRUE,
      cached = TRUE,
      dest = "",
      shiny_html = shinyHTML_with_deps(input, NULL),
      resource_folder = ""))
  }

  # check to see if the file is a Shiny document
  front_matter <- yaml_front_matter(input)
  if (!is_shiny_classic(front_matter$runtime)) {

    # If it's not a Shiny document, then its output is cacheable. Hash the file
    # with its modified date to get a cache key.
    cacheable <- TRUE
    output_key <- digest::digest(paste(input, file.info(input)[4]),
                                 algo = "md5", serialize = FALSE)

    basetmp <- tempdir()

    # On some machines, the TMP directory is /tmp and shared by all users.
    # Caching data under /tmp/rmarkdown would mean that multiple users
    # would try to access that same folder, which probably has permissions
    # 700 by default. So namespace by username.
    username <- Sys.info()[c("effective_user", "user", "login")]
    username <- ifelse(username != "unknown", username, NA_character_)
    username <- na.omit(username)
    username <- head(username, 1)
    if (length(username) == 1) {
      if (!grepl("^[a-z0-9\\-\\_]+$", username, perl = TRUE, ignore.case = TRUE)) {
        # If the user has anything remotely suspicious in their username, use a
        # digest of the username instead, to prevent any possibility of jumping
        # outside of the temp directory.
        username <- substr(digest::digest(username, "md5"), 1, 12)
      }
      basetmp <- file.path(dirname(tempdir()), username)
    }

    output_dest <- paste(file.path(basetmp, "rmarkdown", output_key,
                                   paste("rmd", output_key, sep = "_")),
                         "html", sep = ".")

    # If the output is cacheable, it may also be already cached
    if (file.exists(output_dest)) {
      resource_folder <- knitr_files_dir(output_dest)
      dependencies <- read_shiny_deps(resource_folder)
      shiny_html <- shinyHTML_with_deps(output_dest, dependencies)
      cached <- TRUE
    }
  } else {
    # It's not cacheable, and should be rendered to a session-specific temporary
    # directory, but with a predictable file name.
    tmp_dir <- tempfile()
    output_dest_name <- xfun::with_ext(basename(input), ".html")
    output_dest <- file.path(tmp_dir, output_dest_name)
  }
  list(
    cacheable = cacheable,
    cached = cached,
    dest = output_dest,
    shiny_html = shiny_html,
    resource_folder = resource_folder)
}

# resolve a path relative to a directory (from Shiny)
resolve_relative <- function(dir, relpath) {
  abs.path <- file.path(dir, relpath)
  if (!file.exists(abs.path))
    return(NULL)
  abs.path <- normalize_path(abs.path, mustWork = TRUE)
  dir <- normalize_path(dir, mustWork = TRUE)
  # trim the possible trailing slash under Windows
  if (.Platform$OS.type == 'windows') dir <- sub('/$', '', dir)
  if (nchar(abs.path) <= nchar(dir) + 1)
    return(NULL)
  if (substr(abs.path, 1, nchar(dir)) != dir ||
        substr(abs.path, nchar(dir) + 1, nchar(dir) + 1) != '/') {
    return(NULL)
  }
  return(abs.path)
}

# find 'name' in 'dir', without matching on case (from Shiny)
file.path.ci <- function(dir, name) {
  default <- file.path(dir, name)
  if (file.exists(default))
    return(default)
  if (!dir_exists(dir))
    return(default)

  matches <- list.files(dir, name, ignore.case = TRUE, full.names = TRUE,
                        include.dirs = TRUE)
  # name is used as a pattern above and can match other files
  # so we need to filter as if it was literal string
  matches <- matches[tolower(name) == tolower(basename(matches))]
  if (length(matches) == 0) return(default)
  return(matches[[1]])
}

#' Delay Rendering for an Expression
#'
#' In a Shiny document, evaluate the given expression after the document has
#' finished rendering, instead of during render.
#'
#' This function is useful inside Shiny documents. It delays the
#' evaluation of its argument until the document has finished its initial
#' render, so that the document can be viewed before the calculation is
#' finished.
#'
#' Any expression that returns HTML can be wrapped in \code{render_delayed}.
#' @param expr The expression to evaluate.
#' @return An object representing the expression.
#' @note \code{expr} is evaluated in a \strong{copy} of the environment in which
#'   the \code{render_delayed} call appears. Consequently, no side effects
#'   created by \code{expr} are visible in succeeding expressions, nor are
#'   changes to the environment after the call to \code{render_delayed} visible
#'   to \code{expr}.
#'
#'   \code{expr} must be an expression that produces HTML.
#' @examples
#' \dontrun{
#' # Add the following code to an R Markdown document
#'
#' div(Sys.time())
#'
#' render_delayed({
#'  Sys.sleep(3)      # simulate an expensive computation
#'  div(Sys.time())
#' })
#'
#' div(Sys.time())
#' }
#' @export
render_delayed <- function(expr) {

  # take a snapshot of the environment in which the expr should be rendered
  env <- parent.frame()
  env_snapshot <- new.env(parent = parent.env(env))
  for (var in ls(env, all.names = TRUE))
      assign(var, get(var, env), env_snapshot)

  # take a snapshot of the current knitr and chunk options and the
  # expression to be evaluated
  assign("knitr_cached_chunk_opts", knitr::opts_current$get(), env_snapshot)
  assign("knitr_cached_knit_opts", knitr::opts_knit$get(), env_snapshot)
  assign("knitr_orig_expr", substitute(expr), env_snapshot)

  # evaluate the expression at runtime
  shiny::renderUI(quote({
    knitr::opts_current$restore(knitr_cached_chunk_opts)
    knitr::opts_knit$restore(knitr_cached_knit_opts)
    HTML(knitr::knit_print(eval(knitr_orig_expr), knitr::opts_current$get()))
  }),
  env = env_snapshot,
  quoted = TRUE)
}

is_shiny <- function(runtime, server = NULL) {
  (!is.null(runtime) && grepl('^shiny', runtime)) ||
  is_shiny_prerendered(runtime, server)
}

is_shiny_classic <- function(runtime) {
  identical(runtime, "shiny")
}

is_shiny_prerendered <- function(runtime, server = NULL) {
  if (identical(runtime, "shinyrmd") || identical(runtime, "shiny_prerendered")) {
    TRUE
  } else if (identical(server, "shiny")) {
    TRUE
  } else if (is.list(server) && identical(server[["type"]], "shiny")) {
    TRUE
  } else {
    FALSE
  }
}

write_shiny_deps <- function(files_dir,
                             deps) {
  if (!dir_exists(files_dir)) dir.create(files_dir, recursive = TRUE)
  deps_file <- file.path(files_dir, "dependencies.json")
  deps_json <- jsonlite::serializeJSON(deps, pretty = TRUE)
  write_utf8(deps_json, deps_file)
}

read_shiny_deps <- function(files_dir) {
  deps_path <- file.path(files_dir, "dependencies.json")
  if (file.exists(deps_path)) {
    deps_json <- read_utf8(deps_path)
    dependencies <- jsonlite::unserializeJSON(deps_json)

    # attach rstudio rsiframe script if we are in rstudio
    if (nzchar(Sys.getenv("RSTUDIO")))
      dependencies <- append(dependencies, list(html_dependency_rsiframe()))

    # return
    dependencies
  } else {
    list()
  }
}


# shiny:::setCurrentTheme() was added in 1.6 (we may export in next version)
set_current_theme <- function(theme) {
  set_theme <- asNamespace("shiny")$setCurrentTheme
  if (is.function(set_theme)) set_theme(theme)
}
