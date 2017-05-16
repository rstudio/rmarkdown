md_document_hook <- function(hook) {
  force(hook)

  # regular expression that can capture elements of an image link
  re_link <- paste0(
    "^\\s*",        # initial whitespace
    "!",            # starts with an '!'
    "\\[(.*)\\]",   # '[...]'
    "\\((~/.*)\\)", # '(~/...)'
    "(.*)"          # trailing content (e.g. attributes)
  )

  function(x) {

    # call old hook
    output <- hook(x)

    # split output on newlines
    lines <- unlist(strsplit(output, "\n", fixed = TRUE))
    replaced <- lapply(lines, function(line) {

      # attempt to detect a match (returning original line of failure)
      matches <- gregexpr(re_link, line, perl = TRUE)[[1]]
      if (c(matches) == -1L)
        return(line)

      # extract the used path
      start <- attr(matches, "capture.start")
      length <- attr(matches, "capture.length")
      pieces <- substring(line, start, start + length - 1)

      # extract the path portion
      alt <- pieces[[1]]
      path <- pieces[[2]]
      rest <- pieces[[3]]

      # attempt path expansion
      expanded <- tryCatch(
        path.expand(path),
        error = identity
      )

      if (inherits(expanded, "error"))
        return(line)

      # generate a new line with this expanded path
      fmt <- "![%s](%s)%s"
      sprintf(fmt, alt, expanded, rest)

    })

    # join back into a single string
    paste(replaced, collapse = "\n")
  }
}
