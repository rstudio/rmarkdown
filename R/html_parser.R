# This file implements a simple HTML parser designed to locate tags and
# attribute values in HTML emitted by pandoc. Herein, we process input HTML one 
# character at a time and use a simple state machine to determine where to go 
# go next.

# Parse states--each is a list of rules which govern transitions to other
# states. Any character that doesn't match one of the rules causes the parser
# to stay in the state and store the character as content.
.parse_states <- new.env(parent = emptyenv())

assign("TEXT", list(
  "<" = "NODE"), 
  envir = .parse_states)

assign("NODE", list(
  "!" = "COMMENT", 
  "alpha" = "TAG",
  ">" = "TEXT"), 
  envir = .parse_states)

assign("COMMENT", list(
  ">" = "TEXT"), 
  envir = .parse_states)

assign("TAG", list(
  "white" = "ATTR_LIST",
  ">" = "TEXT"), 
  envir = .parse_states)

assign("ATTR_LIST", list(
  "alpha" = "ATTR",
  ">" = "TEXT"), 
  envir = .parse_states)

assign("ATTR", list(
  "white" = "ATTR_END",
  "=" = "ATTR_VALUE"), 
  envir = .parse_states)

assign("ATTR_END", list(
  "alpha" = "ATTR",
  "=" = "ATTR_VALUE"), 
  envir = .parse_states)

assign("ATTR_VALUE", list(
  "'" = "SINGLE_ATTR",
  "\"" = "DOUBLE_ATTR",
  "white" = "ATTR_VALUE",
  "any" = "UNQUOTED_ATTR"), 
  envir = .parse_states)

assign("SINGLE_ATTR", list(
  "'" = "ATTR_LIST"), 
  envir = .parse_states)

assign("DOUBLE_ATTR", list(
  "\"" = "ATTR_LIST"),
  envir = .parse_states)

assign("UNQUOTED_ATTR", list(
  "white" = "ATTR_LIST",
  ">" = "TEXT"),
  envir = .parse_states)

# Extract HTML attribute values, invoking a callback on each. The callback 
# receives the tag, attribute name, attribute value, and position in the string
# where the value was found.
#
# For instance, extracting values from:
#
#    <img class="bordered" src="foo.png">
#
# results in two callbacks:
#    callback("img", "class", "bordered", ...)
#    callback("img", "src", "foo.png", ...)
#
html_extract_values <- function(html, callback = NULL, show_parse = FALSE) {
  
  # collapse HTML if needed
  if (length(html) > 1) {
    html <- paste(html, collapse = "\n")
  }
  
  # state information (rewritten as we go)
  idx <- 1
  end <- nchar(html)
  state <- get("TEXT", envir = .parse_states)
  contents <- ""
  contents_idx <- idx
  cur_tag <- ""
  cur_attr <- ""
  current_state <- ""
  transition <- FALSE
  
  # function to transition to a new state
  do_transition <- function(new_state) {
    if (current_state == new_state) {
      return(state)
    }
    if (current_state == "TAG")
      cur_tag <<- contents
    else if (current_state == "ATTR")
      cur_attr <<- contents
    else if (!is.null(callback) && 
        (current_state == "DOUBLE_ATTR" || 
         current_state == "SINGLE_ATTR" ||
         current_state == "UNQUOTED_ATTR")) {
      # if the attribute is unquoted then it's already begun
      callback(cur_tag, cur_attr, contents, 
               if (current_state == "UNQUOTED_ATTR")
                 contents_idx - 1 
               else
                 contents_idx)    
    }
    if (show_parse) {
      print(contents)
      print(new_state)
    }
    contents <<- ""
    transition <<- TRUE
    current_state <<- new_state
    contents_idx <<- idx + 1
    get(new_state, envir = .parse_states, inherits = FALSE)
  }
  
  # loop over the contents of the HTML string
  while (idx <= end) {
    # get the character to examine and set up state
    ch <- substr(html, idx, idx)
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
        idx = idx + 1
        break
      }
    }
    # if we didn't transition to a new state, eat the character and move on
    if (!transition) {
      # to save time, only accumulate the contents for states that we're going
      # to report back to the caller
      if (show_parse ||
          current_state %in% c("TAG", "ATTR", "SINGLE_ATTR", "DOUBLE_ATTR", 
                               "UNQUOTED_ATTR")) {
        contents <- paste(contents, ch, sep = "")
      }
      idx = idx + 1
    }
  }
}
