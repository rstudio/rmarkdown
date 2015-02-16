# This file implements a simple HTML parser designed to locate tags and
# attribute values in HTML emitted by pandoc. Herein, we process input HTML one 
# character at a time and use a simple state machine to determine where to go 
# go next.

# Parse states--each is a list of rules which govern transitions to other
# states. Any character that doesn't match one of the rules causes the parser
# to state in the state and store the character as content.
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
  "white" = ".H_ATTR_VALUE",
  "any" = ".H_UNQUOTED_ATTR")

.H_SINGLE_ATTR <- list(
  "'" = ".H_ATTR_LIST")

.H_DOUBLE_ATTR <- list(
  "\"" = ".H_ATTR_LIST")

.H_UNQUOTED_ATTR <- list(
  "white" = ".H_ATTR_LIST",
  ">" = ".H_TEXT")

# Extract HTML attribute values, invoking a callback on each. The callback 
# receives the tag, attribute name, and attribute value.
# For instance, extracting values from:
#    <img class="bordered" src="foo.png">
# results in two callbacks:
#    callback("img", "class", "bordered")
#    callback("img", "src", "foo.png")
html_extract_values <- function(html, callback = NULL, show_parse = FALSE) {
  # state information (rewritten as we go)
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
    if (old_state == new_state) {
      return(state)
    }
    if (old_state == ".H_TAG")
      cur_tag <<- contents
    else if (old_state == ".H_ATTR")
      cur_attr <<- contents
    else if (!is.null(callback) && 
        (old_state == ".H_DOUBLE_ATTR" || 
         old_state == ".H_SINGLE_ATTR" ||
         old_state == ".H_UNQUOTED_ATTR")) {
      callback(cur_tag, cur_attr, contents)    
    }
    if (show_parse) {
      print(contents)
      print(new_state)
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
        # whitespace character--consume it
        state <- do_transition(state[[rule_idx]])
        break
      } else if (identical(pat, "any")) {
        # any character
        state <- do_transition(state[[rule_idx]])
        break
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
