base64_encode_file <- function(in_file, encoder) {

  file_size <- file.info(in_file)$size

  if (file_size <= 0) {
    warning(in_file, 'is empty!')
    return(in_file)
  }
  paste0('data:', mime::guess_type(in_file), ';base64,',
         encoder(readBin(in_file, 'raw', n = file_size)))
}

# processes an HTML resource, given a regular expression that locates
# instances of that resource
process_html_res <- function(html, reg, processor) {
  html <- paste(html, collapse = "\n")
  process_img_src <- function(img_src) {
    src <- sub(reg, '\\1', img_src)
    processor(img_src, src)
  }
  html <- stringr::str_replace_all(html, reg, process_img_src)
  strsplit(html, "\n", fixed = TRUE)[[1]]
}

process_images <- function(html, processor) {
  process_html_res(
    html,
    "<\\s*[Ii][Mm][Gg]\\s+[Ss][Rr][Cc]\\s*=\\s*[\"']([^\"']+)[\"']",
    processor)
}

base64_encode_images <- function(html, encoder) {
  base64_encode_img <- function(img_src, src) {
    in_file <- utils::URLdecode(src)
    if (length(in_file) && file.exists(in_file)) {
      img_src <- sub(src, base64_encode_file(in_file, encoder), img_src,
                     fixed = TRUE)
    }
    img_src
  }
  html <- process_images(html, base64_encode_img)
  process_html_res(html, "<[^>]*style=\"[^\"]*url\\(([^\\)]+)\\)", base64_encode_img)
}

base64_image_encode <- function(data) {
  base64_encode_images(data, base64enc::base64encode)
}


