
.H_TEXT <- list(
  "<" = ".H_NODE")

.H_NODE <- list(
  "!" = ".H_COMMENT", 
  "alpha" = ".H_TAG",
  ">" = ".H_TEXT")

.H_COMMENT <- list(
  ">" = ".H_TEXT")

.H_TAG <- list(
  "white" = ".H_ATTR_LIST",
  ">" = ".H_TEXT")

.H_ATTR_LIST <- list(
  "alpha" = ".H_ATTR",
  ">" = ".H_TEXT")

.H_ATTR <- list(
  "white" = ".H_ATTR_END",
  "=" = ".H_ATTR_VALUE")

.H_ATTR_END <- list(
  "alpha" = ".H_ATTR",
  "=" = ".H_ATTR_VALUE")

.H_ATTR_VALUE <- list(
  "'" = ".H_SINGLE_ATTR",
  "\"" = ".H_DOUBLE_ATTR",
  "any" = ".H_UNQUOTED_ATTR")

.H_SINGLE_ATTR <- list(
  "'" = ".H_ATTR_LIST")

.H_DOUBLE_ATTR <- list(
  "\"" = ".H_ATTR_LIST")

.H_UNQUOTED_ATTR <- list(
  "white" = ".H_ATTR_LIST",
  ">" = ".H_TEXT")

html_extract_values <- function(html, callback) {
  i <- 1
  end <- nchar(html)
  state <- .H_TEXT
  contents <- ""
  cur_tag <- ""
  cur_attr <- ""
  old_state <- ""
  transition <- FALSE
  
  # function to transition to a new state
  do_transition <- function(new_state) {
    if (old_state == ".H_TAG")
      cur_tag <<- contents
    else if (old_state == ".H_ATTR")
      cur_attr <<- contents
    else if (old_state == ".H_DOUBLE_ATTR" || 
        old_state == ".H_SINGLE_ATTR" ||
        old_state == ".H_UNQUOTED_ATTR") {
      callback(cur_tag, cur_attr, contents)    
    }
    contents <<- ""
    transition <<- TRUE
    old_state <<- new_state
    get(new_state)
  }
  
  # loop over the contents of the HTML string
  while (i <= end) {
    # get the character to examine and set up state
    ch <- substr(html, i, i)
    transition <- FALSE
    
    # check each rule
    for (rule_idx in seq_along(state)) {
      
      # extract the pattern to test against
      pat <- names(state)[[rule_idx]]
      
      if (identical(pat, "alpha")) {
        # alphabetic character
        chRaw <- charToRaw(ch)
        if ((chRaw >= charToRaw("a") && chRaw <= charToRaw("z")) ||
            (chRaw >= charToRaw("A") && chRaw <= charToRaw("Z"))) {
          state <- do_transition(state[[rule_idx]])
          break
        }
      } else if (identical(pat, "white") && 
                 (ch == " "  || ch == "\n" || ch == "\t" || ch == "\v")) {
        # whitespace character
        state <- do_transition(state[[rule_idx]])
        break
      } else if (identical(pat, "any")) {
        # any character
        state <- do_transition(state[[rule_idx]])
      } else if (identical(pat, ch)) {
        # specific character--consume it
        state <- do_transition(state[[rule_idx]])
        i = i + 1
        break
      }
    }
    # if we didn't transition to a new state, eat the character and move on
    if (!transition) {
      contents <- paste(contents, ch, sep = "")
      i = i + 1
    }
  }
}
