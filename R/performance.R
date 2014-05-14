perf_timers <- new.env(parent = emptyenv())

elapsed_ms <- function() {
  proc.time()[3] * 1000
}

perf_timer_start <- function(timer_name) {
  assign(timer_name, list(start = elapsed_ms()), perf_timers)
}

perf_timer_stop <- function(timer_name) {
  if (!exists(timer_name, envir = perf_timers))
    return(0)
  timer <- perf_timers[[timer_name]]
  timer$stop = elapsed_ms()
  assign(timer_name, timer, perf_timers)
}

perf_timer_retrieve <- function(timer_name) {
  if (!exists(timer_name, envir = perf_timers))
    return(0)
  timer <- perf_timers[[timer_name]]
  timer$stop - timer$start
}

perf_timer_summary <- function() {
  time <- sapply(ls(perf_timers), function(timer_name) {
    timer <- perf_timers[[timer_name]]
    timer$stop - timer$start
  })
  names(time) <- ls(perf_timers)
  data.frame(time)
}

perf_timers_as_json <- function() {
  summary <- perf_timer_summary()
  json <- paste(lapply(row.names(summary),
                       function(t) { paste(t, ":", as.integer(summary[t,])) } ),
                collapse=", ")
  json <- paste("{", json, "}")
  json
}

