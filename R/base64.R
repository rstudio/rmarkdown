# processes an HTML resource, given a regular expression that locates
# instances of that resource
process_html_res <- function(html, reg, processor) {
  m <- gregexpr(reg, html, perl = TRUE, ignore.case = TRUE)
  regmatches(html, m) <- lapply(regmatches(html, m), function(img_src) {
    src <- sub(reg, '\\1', img_src, ignore.case = TRUE)
    vapply(
      seq_along(img_src),
      function(i) processor(img_src[[i]], src[[i]]),
      character(1)
    )
  })
  html
}

process_images <- function(html, processor) {
  process_html_res(html, "<\\s*img\\s+.*?src\\s*=\\s*[\"']([^\"']+)[\"']", processor)
}

base64_encode_images <- function(html) {
  encode <- function(img_src, src) {
    in_file <- utils::URLdecode(src)
    if (length(in_file) && file.exists(in_file)) {
      img_src <- sub(src, xfun::base64_uri(in_file), img_src, fixed = TRUE)
    }
    img_src
  }
  html <- process_images(html, encode)
  process_html_res(html, "<[^>]*style=\"[^\"]*url\\(([^\\)]+)\\)", encode)
}
