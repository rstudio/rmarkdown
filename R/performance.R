.perf_timers <- new.env(parent = emptyenv())

elapsed_ms <- function() {
  proc.time()[3] * 1000
}

perf_timer_reset_all <- function() {
  .perf_timers <- new.env(parent = emptyenv())
}

perf_timer_start <- function(timer_name) {
  assign(timer_name, list(start = elapsed_ms()), .perf_timers)
}

perf_timer_stop <- function(timer_name) {
  if (!exists(timer_name, envir = .perf_timers))
    return()
  timer <- .perf_timers[[timer_name]]
  timer$stop = elapsed_ms()
  assign(timer_name, timer, .perf_timers)
}

perf_timer_retrieve <- function(timer_name) {
  if (!exists(timer_name, envir = .perf_timers))
    return(0)
  timer <- .perf_timers[[timer_name]]
  timer$stop - timer$start
}

perf_timer_summary <- function() {
  time <- sapply(ls(.perf_timers), function(timer_name) {
    timer <- .perf_timers[[timer_name]]
    timer$stop - timer$start
  })
  names(time) <- ls(.perf_timers)
  data.frame(time)
}

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

create_performance_dependency <- function(files_dir) {
  performance_js <- rmarkdown_system_file("rmd/h/rmd_perf.js")
  js_lines <- readLines(performance_js, warn = FALSE, encoding = "UTF-8")
  js_lines <- gsub("RMARKDOWN_PERFORMANCE_TIMINGS", perf_timers_as_json(),
                   js_lines, fixed = TRUE)
  perf_js_file <- file.path(files_dir, "rmd_perf.js")
  writeLines(js_lines, perf_js_file)
  file.copy(rmarkdown_system_file("rmd/h/rmd_perf.css"),
            file.path(files_dir, "rmd_perf.css"))
  html_dependency(
    name = "rmarkdown-performance",
    version = "0.1",
    path = files_dir,
    script = "rmd_perf.js",
    stylesheet = "rmd_perf.css")
}
