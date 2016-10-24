#' Call a Shiny Module
#'
#' This \code{shiny_module} function provides a shorthand syntax for calling
#' both the UI and server functions of a
#' \href{http://shiny.rstudio.com/articles/modules.html}{Shiny Module} within
#' a \code{runtime: shiny_prerendered} interactive document. Parameters passed
#' to the function will be forwarded as appropriate to the respective UI
#' and Server functions of the Shiny Module.
#'
#' @inheritParams shiny::callModule
#'
#' @param ... Parameters to pass the module UI and server functions.
#'
#' @details The \code{module} parmaeter is the name of the module's server
#'  function. The name of the module's UI function should use the server
#'  function name as it's base and then append either a `\code{UI}` or
#'  \code{_ui} suffix. For example, if the module server function is named
#'  \code{worldPhones} the UI function could be named \code{worldPhonesUI}.
#'
#' @export
shiny_module <- function(module, ...) {

  # capture module as a name (so we can look for an e.g. moduleUI funciton)
  # (trim off any "Server" or "_server" suffix)
  module_server_name <- as.character(substitute(module))
  module_base_name <- sub("(Server|_server)$", "", module_server_name)

  # verify we were passed a function
  if (!is.function(module))
    stop("Specified module '", module, "' is not an R function.")

  # capture args as a list
  args <- list(...)

  # create unique id
  id <- createUniqueId(10)

  # build server args (module + id + matching args from this call)
  server_formals <- formals(module)
  server_args <- args[names(args) %in% names(server_formals)]
  server_args <- append(list(module = as.symbol(module_server_name), id = id), server_args)

  # construct a call to the server function and emit as a shiny_prerendered_chunk
  server_args <- append(as.symbol("callModule"), server_args)
  server_call <- as.call(server_args)
  shiny_prerendered_chunk("server", deparse(server_call))

  # lookup ui function
  ui_func <- NULL
  for (variant in c("UI", "_ui")) {
    func_name <- paste0(module_base_name, variant)
    if (exists(func_name, envir = environment(module))) {
      ui_func <- get(func_name, mode = "function", envir = environment(module))
      break
    }
  }
  if (is.null(ui_func))
    stop("Unable to find module UI function for ", module_base_name)

  # build ui args (id + matching args from this call)
  ui_formals <- formals(ui_func)
  ui_args <- args[names(args) %in% names(ui_formals)]
  ui_args <- append(list(id = id), ui_args)

  # call UI function and return it's output (tags)
  do.call(ui_func, ui_args)
}


