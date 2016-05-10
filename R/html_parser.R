# This function invokes a callback for each resource attribute discovered
# in its first argument. The parameters to the callback are:
#
# 1) The tag discovered (e.g. "img")
# 2) The resource attribute (e.g. "src")
# 3) The attribute's value (e.g. "thumbnail.png")
# 4) The position of the attribute value in the original string (in bytes)
#
call_resource_attrs <- function(html, callback = NULL)  {

  # for performance reasons, we do all regexp matching on literal bytes, and
  # reapply encoding after extracting matches
  html_encoding <- Encoding(html)
  Encoding(html) <- "bytes"

  # use the fastest defaults we can here--PCRE, bytes (RE and input both UTF-8)
  # as the HTML can be quite large
  tags <- gregexpr(
    "<\\s*(img|link|object|script|audio|video|embed|iframe)\\s+([^>]+)>", html,
    perl = TRUE, useBytes = TRUE, ignore.case = TRUE)[[1]]

  for (pos in seq_along(tags)) {
    # extract the HTML tag in question
    tagstart <- attr(tags, "capture.start", exact = TRUE)[pos,1]
    tag <- substr(html, tagstart,  tagstart +
                    attr(tags, "capture.length", exact = TRUE)[pos,1] - 1)
    Encoding(tag) <- html_encoding
    tag <- tolower(tag)

    # determine the attribute to look for based on the tag name
    tagattr <- switch(tag,
                      img    = "src",
                      link   = "href",
                      object = "data",
                      script = "src",
                      audio  = "src",
                      video  = "src",
                      embed  = "src",
                      iframe = "src")

    # extract the tags
    attrstart <- attr(tags, "capture.start", exact = TRUE)[pos,2]
    attrs <- substr(html, attrstart, attrstart +
                    attr(tags, "capture.length", exact = TRUE)[pos,2] - 1)

    # see if one of the attributes if the one we're looking for
    attrmatch <- regexpr(paste0("\\s*", tagattr,
                                "\\s*=\\s*('[^']+'|\"[^\"]+\")"),
                         attrs, ignore.case = TRUE, perl = TRUE,
                         useBytes = TRUE)
    if (attrmatch >= 0) {
      matchstart <- attrstart +
        attr(attrmatch, "capture.start", exact = TRUE) - 1
      matchlength <- attr(attrmatch, "capture.length", exact = TRUE)

      # before we attempt to extract the entire attribute value, check to see
      # if it's a large data blob; if it is, skip it. note that substr() is
      # very expensive on long strings so this is cheaper than just doing a
      # prefix check immediately.
      if (matchlength > 256) {
        if (substr(html, matchstart + 1, matchstart + 5) == "data:") {
          next
        }
      }

      # extract the matching bytes, re-apply encoding, and invoke the callback
      resource <- substr(html, matchstart + 1, matchstart + matchlength - 2)
      Encoding(resource) <- html_encoding
      callback(tag, tagattr, resource, matchstart + 1)
    }
  }
}

# as call_resource_attrs, but for CSS
call_css_resource_attrs <- function(css, callback = NULL)  {
  css_encoding <- Encoding(css)
  Encoding(css) <- "bytes"

  urls <- gregexpr("url\\s*\\(\\s*['\"]?([^'\")]+)['\"]?\\s*\\)", css,
                   perl = TRUE, useBytes = TRUE, ignore.case = TRUE)[[1]]
  for (pos in seq_along(urls)) {
    urlstart <- attr(urls, "capture.start", exact = TRUE)[pos,1]
    url <- substr(css, urlstart,  urlstart +
                    attr(urls, "capture.length", exact = TRUE)[pos,1] - 1)
    Encoding(url) <- css_encoding
    callback("css", "url", url, urlstart)
  }
}
