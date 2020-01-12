list_builder <- function() {
  (function() {
    capacity_ <- 1024
    index_ <- 0
    data_ <- vector("list", capacity_)

    append <- function(data) {

      # increment index and check capacity
      index_ <<- index_ + 1
      if (index_ > capacity_) {
        capacity_ <<- capacity_ * 2
        data_[capacity_] <<- list(NULL)
      }

      # append data
      if (is.null(data))
        data_[index_] <<- list(NULL)
      else
        data_[[index_]] <<- data
    }

    data <- function() {
      data_[seq_len(index_)]
    }

    clear <- function() {
      capacity_ <<- 1024
      index_ <<- 0
      data_ <<- vector("list", capacity_)
    }

    empty <- function() {
      index_ == 0
    }

    list(
      append = append,
      clear = clear,
      empty = empty,
      data = data
    )

  })()
}
