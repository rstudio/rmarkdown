
library(htmltools)

# Generate HTML for a 4-wide bootstrap thumbnail
thumbnail <- function(title, img, href, caption = TRUE) {
  div(class = "col-sm-4",
      a(class = "thumbnail", title = title, href = href,
        img(src = img),
        div(class = ifelse(caption, "caption", ""),
          ifelse(caption, title, "")
        )
      )
  )
}

# Generate HTML for several rows of 4-wide bootstrap thumbnails 
thumbnails <- function(thumbs) {
  
  # capture arguments and setup rows to return
  numThumbs <- length(thumbs)
  fullRows <- numThumbs / 3
  rows <- tagList()
  
  # add a row of thumbnails
  addRow <- function(first, last) {
    rows <<- tagAppendChild(rows, div(class = "row", thumbs[first:last]))
  }
  
  # handle full rows
  for (i in 1:fullRows) {
    last <- i * 3
    first <- last-2
    addRow(first, last)
  }
  
  # check for leftovers
  leftover <- numThumbs %% 3
  if (leftover > 0) {
    last <- numThumbs
    first <- last - leftover + 1
    addRow(first, last)
  }
  
  # return the rows!
  rows
}

# Generate HTML for examples
examples <- function(caption = TRUE, showcaseOnly = FALSE, shinyOnly = FALSE) {
  
  # read examples into data frame (so we can easily sort/filter/etc)
  examples <- yaml::yaml.load_file("examples.yml")
  examples <- plyr::ldply(examples, data.frame, stringsAsFactors=FALSE)
  
  # filter if requested
  if (showcaseOnly)
    examples <- subset(examples, examples$showcase == TRUE)
  if (shinyOnly)
    examples <- subset(examples, examples$shiny == TRUE)
  
  # convert to list for thumbnail generation
  examples <- apply(examples, 1, function(r) { 
    list(title = r["title"],
         img = r["img"],
         href = r["href"]) 
  })
  
  thumbnails(lapply(examples, function(x) {
    thumbnail(title = x$title, 
              img = x$img, 
              href = x$href, 
              caption = caption)
  }))
}


