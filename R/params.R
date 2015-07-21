
knit_params_get <- function(input_lines, params) {
  # check for recent enough knitr
  if (packageVersion("knitr") < "1.10") {
    stop("knitr >= 1.10 required to use rmarkdown params")
  }
  
  # read the default parameters and extract them into a named list
  knit_params <- mark_utf8(knitr::knit_params(input_lines))
  default_params <- list()
  for (param in knit_params) {
    default_params[[param$name]] <- param$value
  }
  
  # validate params passed to render
  if (!is.null(params)) {

    if (identical(params, "ask")) {
      if (!interactive()) {
        stop("render parameter configuration only allowed in an interactive environment")
      }
      
      params <- knit_params_ask(input_lines = input_lines)
      if (is.null(params)) {
        stop("render parameter configuration canceled")
      }
    }
    
    # verify they are a list
    if (!is.list(params) || (length(names(params)) != length(params))) {
      stop("render params argument must be a named list")
    }
    
    # verify that all parameters passed are also in the yaml
    invalid_params <- setdiff(names(params), names(default_params))
    if (length(invalid_params) > 0) {
      stop("render params not declared in YAML: ",
           paste(invalid_params, sep = ", "))
    }
  }
  
  # merge explicitly provided params with defaults
  merge_lists(default_params, params, recursive = FALSE)
}

params_converters <- list(
    logical = function(value) {
      as.logical(value)
    },
    Date = function(value) {
      as.Date(value)
    },
    POSIXct = function(value) {
      ## FIXME: This is how knitr does datetime conversion, but maybe we should use the local tz.
      as.POSIXct(value, tz="GMT")
    },
    integer = function(value) {
      as.integer(value)
    },
    numeric = function(value) {
      as.numeric(value)
    },
    character = function(value) {
      as.character(value)
    },
    data.frame = function(value) {
      read.csv(value$datapath)
    }
    )

params_get_converter <- function(param) {
  for (c in param$class) {
    converter <- params_converters[[c]]
    if (!is.null(converter)) {
      return(converter)
    }
  }
  ## FIXME: this probably should be an error, as it is an unsupported type.
  return(function(value){ value })
}

params_controls <- list()
params_controls$integer <- params_controls$numeric <- function(param) {
  ## If min/max are specified, use a slider.
  if (is.null(param$min) || is.null(param$max)) {
    shiny::numericInput
  } else {
    shiny::sliderInput
  }
}

params_controls$logical <- function(param) { shiny::checkboxInput }
## BUG: dateInput does not allow the user to not specify a value.
##     https://github.com/rstudio/shiny/issues/896
params_controls$Date <- function(param) { shiny::dateInput }
## BUG: shiny does not support datetime selectors
##     https://github.com/rstudio/shiny/issues/897
##     we ask for string input for now.
params_controls$POSIXct <- function(param) { shiny::textInput }
params_controls$character <- function(param) { shiny::textInput }
params_controls$data.frame <- function(param) { shiny::fileInput }
params_get_control <- function(param) {
  ## A value might have multiple classes. Try: class(Sys.time())
  ## Try to find first class listed with a named control.
  for (c in param$class) {
    control <- params_controls[[c]]
    if (!is.null(control)) {
      return(control(param))
    }
  }
  return(NULL)
}

params_configurable <- function(param) {
  (!is.null(params_get_control(param))) && ( length(param$value) == 1 || "data.frame" %in% param$class )
}

#' Run a shiny application asking for parameter configuration for the given document.
#'
#' @param file Path to the R Markdown document with configurable parameters.
#' @param input_lines Content of the R Markdown document. If \code{NULL}, the contents of \code{file} will be read.
#' @param params A named list of optional parameter overrides used in place of the document defaults.
#' @param shiny_args Additional arguments to \code{\link[shiny:runApp]{runApp}}.
#'
#' @return named list with overridden parameter names and value.
#' 
#' @export
knit_params_ask <- function(file = "index.Rmd",
                            input_lines = NULL,
                            params = NULL,
                            shiny_args = NULL,
                            encoding = getOption("encoding")) {
  if (packageVersion("knitr") < "1.10.17") {
    stop("knitr >= 1.10.17 required to use rmarkdown::knit_params_ask")
  }

  if (is.null(input_lines)) {
    input_lines <- read_lines_utf8(file, encoding)
  }
  
  knit_params <- mark_utf8(knitr::knit_params(input_lines))

  ## Input validation on params (checks shared with render)
  if (!is.null(params)) {
    ## Must be a named list
    if (!is.list(params) || (length(names(params)) != length(params))) {
      stop("knit_params_ask params argument must be a named list")
    }

    ## Verify that all passed parameters are also in the yaml
    invalid_params <- setdiff(names(params), names(knit_params))
    if (length(invalid_params) > 0) {
      stop("knit_params_ask params not declared in YAML: ",
           paste(invalid_params, sep = ", "))
    }
  }

  ## If we happen to not have any knit_params, just return an empty list and
  ## don't fire up the Shiny app.
  if (length(knit_params) == 0) {
    return(list())
  }
  
  configurable <- Filter(params_configurable, knit_params)
  unconfigurable <- Filter(Negate(params_configurable), knit_params)

  ## Return the current value for a given parameter, which is either `params`
  ## when overridden by our caller or `param$value` otherwise.
  current_value <- function(param) {
    if (!is.null(params)) {
      override <- params[[param$name]]
      if (!is.null(override)) {
        return(override)
      }
    }
    return(param$value)
  }

  ## The default values given to the UI. Lets us detect if the user has
  ## specified an override value or is letting it float to whatever default is
  ## generated by the YAML (most useful for R expressions).
  ##
  ## Only items in `configurable` will be present here.
  uidefaults <- list()

  server <- function(input, output) {
    param.ui <- function(param) {
      inputId <- paste0(param$name)
      label <- ifelse(is.null(param$label), param$name, param$label)
      
      arguments = list(
          inputId = inputId,
          label = label
          )
      
      ## A number of controls support min/max (numeric, slider, date).
      ## Blindly support a bunch of options and assume that the user knows
      ## what options are used by what controle.
      if (!is.null(param$min)) {
        arguments$min <- param$min
      }
      if (!is.null(param$max)) {
        arguments$max <- param$max
      }
      if (!is.null(param$step)) {
        arguments$step <- param$step
      }
      
      ## TODO: if long input, truncate for display
      
      inputControlFn <- params_get_control(param)

      if (!is.null(param$choices)) {
        ## radio buttons for a small number of choices, select otherwise.
        if (length(param$choices) <= 4) {
          inputControlFn <- shiny::radioButtons
          arguments$choices <- param$choices
        } else {
          inputControlFn <- shiny::selectInput
          arguments$choices <- param$choices
        }
      } else {
        ## Not choices.
        
      }

      inputControlFnFormals <- names(formals(inputControlFn))
      if ("value" %in% inputControlFnFormals) {
        arguments$value <- current_value(param)
      } else if ("selected" %in% inputControlFnFormals) {
        ## BUG: What if the value is not in the choices?
        arguments$selected <- current_value(param)
      }
      uidefaults[[param$name]] <<- {
        ## data.frame uses a file dialog which gives a NULL value when no file
        ## is provided.
        if ("data.frame" %in% param$class) {
          NULL
        } else {
          ## This is not current_value(param) because we want to understand
          ## deviation from the report default, not the call-time override.
          param$value
        }
      }
      
      inputControl <- do.call(inputControlFn, arguments)
      useSelectControl <- function(current) { FALSE }
      selectControl <- NULL

      ## Dates and times with expressions that mean "now" or "today" are first
      ## materialized as selects. If the user chooses to customize the field,
      ## we then show the type-specific picker.
      if (is.null(params[[param$name]])) { # prior value; implicit customization
        if ("POSIXct" %in% param$class && identical("Sys.time()", param$expr)) {
          useSelectControl <- function(current) {
            (is.null(current) || identical(current, "default"))
          }
          uidefaults[[param$name]] <<- "default"
          choices <- list()
          choices[[paste0("now (",param$value,")")]] <- "default"
          choices[["Use a custom time"]] <- "custom"
          selectControl <- shiny::selectInput(inputId = inputId,
                                              label = label,
                                              choices = choices)
        } else if ("Date" %in% param$class && identical("Sys.Date()", param$expr)) {
          useSelectControl <- function(current) {
            (is.null(current) || identical(current, "default"))
          }
          uidefaults[[param$name]] <<- "default"
          choices <- list()
          choices[[paste0("today (",param$value,")")]] <- "default"
          choices[["Use a custom date"]] <- "custom"
          selectControl <- shiny::selectInput(inputId = inputId,
                                              label = label,
                                              choices = choices)
        }
      }
      
      output[[paste0("ui_", param$name)]] <- shiny::renderUI({
        if (useSelectControl(input[[param$name]])) {
          selectControl
        } else {
          inputControl
        }
      })
    }

    lapply(configurable, function(param) {
      param.ui(param)
    })
    
    values <- shiny::eventReactive(input$save, {
      values <- list()
      lapply(configurable, function(param) {
        default <- uidefaults[[param$name]]
        value <- input[[param$name]]
        if (!identical(value, default)) {
          values[[param$name]] <<- params_get_converter(param)(value)
        }
      })
      ## This set of published values is the raw set that came from the user.
      ## It does not include those values that cannot be configured or are
      ## left to use the default.
      values
    })

    shiny::observe({
      shiny::stopApp(values())
    })

    shiny::observeEvent(input$cancel, {
      shiny::stopApp(NULL)
    })
  }

  contents <- shiny::tagList(
      shiny::fluidRow(shiny::column(12,shiny::tags$h1("Configure Report Parameters"))),
      shiny::fluidRow(shiny::column(12, lapply(configurable, function(param) {
        shiny::uiOutput(paste0("ui_", param$name))
      }))),
      shiny::fluidRow(shiny::column(12,shiny::actionButton("cancel","Cancel"), shiny::actionButton("save", "Save", class="btn-primary"))))
  
  if (length(unconfigurable) > 0) {
    contents <- shiny::tagAppendChildren(contents, 
                                         shiny::fluidRow(shiny::column(12,h2("Parameters that cannot be configured"))),
                                         shiny::fluidRow(shiny::column(12,shiny::tags$ul(lapply(unconfigurable, function(param) { shiny::tags$li(param$name) })))))
  }
  contents <- shiny::tagAppendChild(contents, shiny::fluidRow(shiny::column(12,shiny::textOutput("values"))))
  ui <- shiny::fluidPage(
      shiny::tags$head(shiny::tags$style(".container-fluid .shiny-input-container { width: auto; }")),
      contents)

  shiny_app <- shiny::shinyApp(ui = ui, server = server)
  shiny_args <- merge_lists(list(appDir = shiny_app), shiny_args)
  do.call(shiny::runApp, shiny_args)
}
