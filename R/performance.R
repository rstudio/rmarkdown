# Supporting functions for performance timers. These are primarily used to make
# performance information available in Shiny documents (see ::run), but can
# also display a summary of performance after any render:
#
# > rmarkdown::render( ... )
# > rmarkdown:::perf_timer_summary()

# private environment to store performance timers
.perf_timers <- new.env(parent = emptyenv())

# convenience for returning ms elapsed since the R session began
elapsed_ms <- function() {
  proc.time()[3] * 1000
}

# clears all perf timer state
perf_timer_reset_all <- function() {
  .perf_timers <- new.env(parent = emptyenv())
}

# record a start time for a perf timer
perf_timer_start <- function(timer_name) {
  assign(timer_name, list(start = elapsed_ms()), .perf_timers)
}

# record a stop time for a perf timer
perf_timer_stop <- function(timer_name) {
  if (!exists(timer_name, envir = .perf_timers))
    return()
  timer <- .perf_timers[[timer_name]]
  timer$stop = elapsed_ms()
  assign(timer_name, timer, .perf_timers)
}

# return a formatted data frame with a performance timer summary
perf_timer_summary <- function() {
  time <- sapply(ls(.perf_timers), function(timer_name) {
    timer <- .perf_timers[[timer_name]]
    timer$stop - timer$start
  })
  names(time) <- ls(.perf_timers)
  data.frame(time)
}

# return a formatted JSON string with a performance timer summary
# (for use in rmd_perf.js)
perf_timers_as_json <- function() {
  summary <- perf_timer_summary()
  json <- paste(lapply(row.names(summary), function(t) {
                         paste("{ marker: '", t, "',",
                                " elapsed: ", as.integer(summary[t,]), " }",
                               sep = "")
                         }),
                collapse=", ")
  json <- paste("[", json, "]")
  json
}

# inject performance timings into rmd_perf.js; write performance timing
# supporting files to files_dir and return an HTML dependency object suitable
# for inclusion in the document for which the timings were collected
create_performance_dependency <- function(files_dir) {
  performance_js <- rmarkdown_system_file("rmd/h/rmarkdown/rmd_perf.js")
  js_lines <- readLines(performance_js, warn = FALSE, encoding = "UTF-8")
  js_lines <- gsub("RMARKDOWN_PERFORMANCE_TIMINGS", perf_timers_as_json(),
                   js_lines, fixed = TRUE)
  perf_js_file <- file.path(files_dir, "rmd_perf.js")
  writeLines(js_lines, perf_js_file)
  file.copy(rmarkdown_system_file("rmd/h/rmarkdown/rmd_perf.css"),
            file.path(files_dir, "rmd_perf.css"))
  htmlDependency(
    name = "rmarkdown-performance",
    version = "0.1",
    src = files_dir,
    script = "rmd_perf.js",
    stylesheet = "rmd_perf.css")
}
