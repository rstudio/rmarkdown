# processes an HTML resource, given a regular expression that locates
# instances of that resource
process_html_res <- function(html, reg, processor) {
  html <- one_string(html)
  process_img_src <- function(img_src) {
    src <- sub(reg, '\\1', img_src)
    vapply(
      seq_along(img_src),
      function(i) processor(img_src[[i]], src[[i]]),
      character(1)
    )
  }
  html <- stringr::str_replace_all(html, reg, process_img_src)
  strsplit(html, "\n", fixed = TRUE)[[1]]
}

process_images <- function(html, processor) {
  process_html_res(
    html,
    "<\\s*[Ii][Mm][Gg]\\s+.*?[Ss][Rr][Cc]\\s*=\\s*[\"']([^\"']+)[\"']",
    processor)
}

base64_encode_images <- function(html) {
  base64_encode_img <- function(img_src, src) {
    in_file <- utils::URLdecode(src)
    if (length(in_file) && file.exists(in_file)) {
      img_src <- sub(src, xfun::base64_uri(in_file), img_src, fixed = TRUE)
    }
    img_src
  }
  html <- process_images(html, base64_encode_img)
  process_html_res(html, "<[^>]*style=\"[^\"]*url\\(([^\\)]+)\\)", base64_encode_img)
}
