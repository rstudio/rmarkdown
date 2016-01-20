
knit_params_get <- function(input_lines, params) {
  # check for recent enough knitr
  if (utils::packageVersion("knitr") < "1.10") {
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

params_label <- function(inputControlFn, param) {
  label <- ifelse(is.null(param$label), param$name, param$label)
  if (identical(inputControlFn, shiny::fileInput)) {
    if (is.character(param$value)) {
      label <- paste0(label, " (default: ", param$value, ")")
    }
  }
  label
}

params_value_to_ui <- function(inputControlFn, value) {
  if (identical(inputControlFn, shiny::fileInput)) {
    NULL
  } else if (identical(inputControlFn, shiny::textInput)) {
    ## TODO: if long input, maybe truncate textInput values for display

    classes <- class(value)
    if ("POSIXct" %in% classes) {
      as.character(value)
    } else {
      value
    }
  } else if (is.null(value)) {
    numerics <- c(shiny::numericInput,
                  shiny::sliderInput)
    # The numerics can't deal with a NULL value, but everything else is fine.
    if (identical(inputControlFn, shiny::numericInput) ||
        identical(inputControlFn, shiny::sliderInput)) {
      0
    } else {
      value
    }
  } else {
    ## A type/control that doesn't need special handling; just emit the value.
    value
  }
}

params_value_from_ui <- function(inputControlFn, value, uivalue) {
  if (identical(inputControlFn, shiny::fileInput)) {
    uivalue$datapath
  } else if (identical(inputControlFn, shiny::textInput)) {
    classes <- class(value)
    if ("POSIXct" %in% classes) {
      as.POSIXct(uivalue)
    } else {
      uivalue
    }
  } else {
    ## A type/control that doesn't need special handling; just emit the value.
    uivalue
  }
}

params_get_input <- function(param) {
  # Maps between value types and input: XXX
  default_inputs <- list(
      logical = "checkbox",
      ## BUG: dateInput does not allow the user to not specify a value.
      ##     https://github.com/rstudio/shiny/issues/896
      Date = "date",
      ## BUG: shiny does not support datetime selectors
      ##     https://github.com/rstudio/shiny/issues/897
      ##     we ask for string input for now.
      POSIXct = "datetime",
      character = "text"
      )
  default_inputs$integer <- default_inputs$numeric <-  {
    ## If min/max are specified, use a slider.
    if (is.null(param$min) || is.null(param$max)) {
      "numeric"
    } else {
      "slider"
    }
  }

  input <- param$input
  if (is.null(input)) {
    if (!is.null(param$choices)) {
      ## radio buttons for a small number of choices, select otherwise.
      if (length(param$choices) <= 4) {
        input <- "radio"
      } else {
        input <- "select"
      }
    } else {
      ## Not choices. Look at the value type to find what input control we
      ## should use.

      ## A value might have multiple classes. Try: class(Sys.time())
      ## Try to find first class listed with a named control.
      for (c in class(param$value)) {
        default_input <- default_inputs[[c]]
        if (!is.null(default_input)) {
          input <- default_input
          break
        }
      }
    }
  }
  input
}

params_get_control <- function(param) {
  input <- params_get_input(param)
  if (is.null(input)) {
    return(NULL)
  }

  # Maps between input: XXX and the various Shiny input controls
  input_controls <- list(
      checkbox = shiny::checkboxInput,
      numeric  = shiny::numericInput,
      slider   = shiny::sliderInput,
      date     = shiny::dateInput,
      datetime = shiny::textInput, # placeholder for future datetime picker
      text     = shiny::textInput,
      file     = shiny::fileInput,
      radio    = shiny::radioButtons,
      select   = shiny::selectInput
      )
  control <- input_controls[[input]]
  if (is.null(control)) {
    stop(paste("could not determine what control to use for parameter", param$name, "with input:", input))
  }
  control
}

params_configurable <- function(param) {
  length(param$value) <= 1 && !is.null(params_get_control(param))
}

# Returns a new empty named list.
params_namedList <- function() {
  empty <- list()
  names(empty) <- character()
  empty
}

#' Run a shiny application asking for parameter configuration for the given document.
#'
#' @param file Path to the R Markdown document with configurable parameters.
#' @param input_lines Content of the R Markdown document. If \code{NULL}, the contents of \code{file} will be read.
#' @param params A named list of optional parameter overrides used in place of the document defaults.
#' @param shiny_args Additional arguments to \code{\link[shiny:runApp]{runApp}}.
#' @param save_caption Caption to use use for button that saves/confirms parameters.
#' @param encoding The encoding of the input file; see \code{\link{file}}.
#'
#' @return named list with overridden parameter names and value.
#'
#' @export
knit_params_ask <- function(file = NULL,
                            input_lines = NULL,
                            params = NULL,
                            shiny_args = NULL,
                            save_caption = "Save",
                            encoding = getOption("encoding")) {
  if (utils::packageVersion("knitr") < "1.10.18") {
    stop("knitr >= 1.10.18 required to use rmarkdown::knit_params_ask")
  }

  if (is.null(input_lines)) {
    if (is.null(file)) {
      stop("knit_params_ask must have a non-NULL file or input_lines parameter")
    }
    input_lines <- read_lines_utf8(file, encoding)
  }

  knit_params <- mark_utf8(knitr::knit_params(input_lines))

  ## Input validation on params (checks shared with render)
  if (!is.null(params)) {
    ## Must be a named list
    if (!is.list(params) || (length(names(params)) != length(params))) {
      stop("knit_params_ask params argument must be a named list")
    }
    ## We do not validate names(params) because the document may have changed
    ## but we're loading parameters that were configured with an older
    ## version.
  }

  ## If we happen to not have any knit_params, just return an empty named list
  ## and don't fire up the Shiny app.
  if (length(knit_params) == 0) {
    return(params_namedList())
  }

  configurable <- Filter(params_configurable, knit_params)
  unconfigurable <- Filter(Negate(params_configurable), knit_params)

  ## This set of published values is the raw set that came from the user.
  ## It does not include those values that cannot be configured or are
  ## left to use the default.
  values <- params_namedList()

  server <- function(input, output) {
    param.ui <- function(param) {
      inputControlFn <- params_get_control(param)
      inputControlFnFormals <- names(formals(inputControlFn))

      inputId <- param$name
      label <- params_label(inputControlFn, param)

      arguments = list(
          inputId = inputId,
          label = label
          )

      # We MUST process the "value" name even if it is not present (due to
      # NULL values).
      attrib_names <- unique(c(names(param), "value"))
      lapply(attrib_names, function(name) {
        if (name %in% c("name", "input", "expr")) {
        } else if (name == "label") {
          arguments$label <<- label
        } else if (name == "value") {

          ## The current value for this parameter is either `params` when
          ## overridden by our caller or `param$value` otherwise.
          current_value <- param$value
          if (!is.null(params)) {
            override <- params[[param$name]]
            if (!is.null(override)) {
              current_value <- override
            }
          }
          # Now, transform into something that the input control can handle.
          current_value <- params_value_to_ui(inputControlFn, current_value)

          # value maps to either "value" or "selected" depending on the control.
          if ("value" %in% inputControlFnFormals) {
            arguments$value <<- current_value
          } else if ("selected" %in% inputControlFnFormals) {
            arguments$selected <<- current_value
          }
        } else {
          ## Not a special field. Blindly promote to the input control.
          arguments[[name]] <<- param[[name]]
        }
      })

      ## This is based on param$value not current_value because we want to
      ## understand deviation from the report default, not any (optional)
      ## call-time override.
      uidefault <- params_value_to_ui(inputControlFn, param$value)
      hasDefaultValue <- function(value) {
        identical(uidefault, value)
      }

      inputControl <- NULL
      unsupported <- setdiff(names(arguments), inputControlFnFormals)
      if (length(unsupported) > 0) {
        inputControl <- shiny::div(class = "form-group",
                                   tags$label(class="control-label",param$name),
                                   shiny::div(paste('Cannot customize the parameter "', param$name, '" ',
                                                    'because the "', params_get_input(param), '" ',
                                                    'Shiny control does not support: ',
                                                    paste(unsupported, collapse = ', '), sep = '')))
      } else {
        inputControl <- do.call(inputControlFn, arguments)
      }

      showSelectControl <- NULL
      selectControl <- NULL
      selectInputId <- paste0("select_", param$name)

      ## Helper to materialize a "default/customize" control.
      makeSelectControl <- function(default_name, custom_name) {
        showSelectControl <<- function(current) {
          (is.null(current) || identical(current, "default"))
        }
        hasDefaultValue <<- function(value) { FALSE }
        choices <- list()
        choices[[default_name]] <- "default"
        choices[[custom_name]] <- "custom"
        selectControl <<- shiny::selectInput(inputId = selectInputId,
                                             label = label,
                                             choices = choices)
      }

      if (is.null(params[[param$name]])) { # prior value; implicit customization
        ## Dates and times with expressions that mean "now" or "today" are first
        ## materialized as selects. If the user chooses to customize the field,
        ## we then show the type-specific picker.
        if (identical("Sys.time()", param$expr)) {
          makeSelectControl(paste0("now (", param$value, ")"),
                            "Use a custom time")
        } else if (identical("Sys.Date()", param$expr)) {
          makeSelectControl(paste0("today (", param$value, ")"),
                            "Use a custom date")
        } else if (is.null(param$value)) {
          # fileInput defaults to null, but for other null values, ask the
          # user to explicitly choose to override (ie. we cannot use value
          # comparison).
          if (!identical(inputControlFn, shiny::fileInput)) {
            makeSelectControl("Unspecified (NULL)",
                              "Use a custom value")
          }
        }
      }

      output[[paste0("ui_", param$name)]] <- shiny::renderUI({
        # For most parameters, the selectInputId input will be NULL.
        if (!is.null(showSelectControl) && showSelectControl(input[[selectInputId]])) {
          selectControl
        } else {
          inputControl
        }
      })

      shiny::observe({
        # A little reactive magic to keep in mind. If you're in one of the
        # "default/custom" selector scenarios, this will never fire until the
        # user selects "custom" because hte value-producing input control is
        # not rendered until that point.
        uivalue <- input[[param$name]]
        if (is.null(uivalue)) {
          # ignore startup NULLs
        } else if (hasDefaultValue(uivalue)) {
          values[[param$name]] <<- NULL
        } else {
          values[[param$name]] <<- params_value_from_ui(inputControlFn, param$value, uivalue)
        }
      })
    }

    lapply(configurable, function(param) {
      param.ui(param)
    })

    shiny::observeEvent(input$save, {
      shiny::stopApp(values)
    })

    shiny::observeEvent(input$cancel, {
      shiny::stopApp(NULL)
    })
  }

  contents <- shiny::tags$div(
      shiny::fluidRow(shiny::column(12, lapply(configurable, function(param) {
        shiny::uiOutput(paste0("ui_", param$name))
      }))), class = "container-fluid")

  if (length(unconfigurable) > 0) {
    skipped <- shiny::tags$div(shiny::tags$strong("Note:"),
                            "The following parameters cannot be customized:",
                            paste(lapply(unconfigurable, function(param) { param$name }), collapse = ", "))
    contents <- shiny::tagAppendChildren(contents, shiny::fluidRow(shiny::column(12, skipped)))
  }
  footer <- shiny::tags$div(
      shiny::tags$div(
          shiny::fluidRow(shiny::column(12,
                                        shiny::actionButton("save", save_caption, class = "btn-primary navbar-btn pull-right"),
                                        shiny::actionButton("cancel","Cancel", class = "navbar-btn pull-right"))),
          class = "container-fluid"),
      class = "navbar navbar-default navbar-fixed-bottom")

  style <- shiny::tags$style(
      # Our controls are wiiiiide.
      ".container-fluid .shiny-input-container { width: auto; }",
      # Prevent the save/cancel buttons from squashing together.
      ".navbar button { margin-left: 10px; }",
      # Style for the navbar footer.
      # http://getbootstrap.com/components/#navbar-fixed-bottom
      "body { padding-bottom: 70px; }"
                             )
  ## Escape is "cancel" and Enter is "save".
  script <- shiny::tags$script(shiny::HTML("$(document).keyup(function(e) {\n",
                                           "if (e.which == 13) { $('#save').click(); } // enter\n",
                                           "if (e.which == 27) { $('#cancel').click(); } // esc\n",
                                           "});"
                                           ))
  ui <- shiny::bootstrapPage(
      shiny::tags$head(style, script),
      contents,
      footer)

  shiny_app <- shiny::shinyApp(ui = ui, server = server)
  shiny_args <- merge_lists(list(appDir = shiny_app), shiny_args)
  do.call(shiny::runApp, shiny_args)
}
