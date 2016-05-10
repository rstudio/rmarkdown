new_stack <- function() {

  .data  <- list()
  .index <- 0

  push <- function(data) {
    .index <<- .index + 1
    .data[[.index]] <<- data
  }

  pop <- function() {
    if (.index == 0)
      return(NULL)

    result <- .data[[.index]]
    .index <<- .index - 1
    result
  }

  peek <- function() {
    if (.index == 0)
      return(NULL)
    .data[[.index]]
  }

  list(push = push, pop = pop, peek = peek)
}

