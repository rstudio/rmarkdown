
.MIMEMAP <- list(

  ## most common web types
  htm = 'text/html',
  html = 'text/html',
  css = 'text/css',
  gif = 'image/gif',
  jpg = 'image/jpeg',
  jpeg = 'image/jpeg',
  jpe = 'image/jpeg',
  png = 'image/png',
  js = 'application/x-javascript',
  pdf = 'application/pdf',
  svg = 'image/svg+xml',
  swf = 'application/x-shockwave-flash',

  ## markdown types
  md = 'text/x-markdown',
  mdtxt = 'text/x-markdown',
  markdown = 'text/x-markdown',

  ## other types we are likely to serve
  xml = 'text/xml',
  csv = 'text/csv',
  ico = 'image/x-icon',
  zip = 'application/zip',
  bz = 'application/x-bzip',
  bz2 = 'application/x-bzip2',
  gz = 'application/x-gzip',
  tar = 'application/x-tar',

  ## yet more types...

  shtml = 'text/html',
  tsv = 'text/tab-separated-values',
  tab = 'text/tab-separated-values',
  dcf = 'text/debian-control-file',
  txt = 'text/plain',
  mml = 'text/mathml',

  tif = 'image/tiff',
  tiff = 'image/tiff',
  bmp = 'image/bmp',
  ps = 'application/postscript',
  eps = 'application/postscript',
  dvi =   'application/x-dvi',

  atom = 'application/atom+xml',
  rss = 'application/rss+xml',

  doc = 'application/msword',
  docx = 'application/vnd.openxmlformats-officedocument.wordprocessingml.document',
  odt = 'application/vnd.oasis.opendocument.text',
  rtf = 'application/rtf',
  xls = 'application/vnd.ms-excel',
  xlsx = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
  ods = 'application/x-vnd.oasis.opendocument.spreadsheet',
  ppt = 'application/vnd.ms-powerpoint',
  pps = 'application/vnd.ms-powerpoint',
  pptx = 'application/vnd.openxmlformats-officedocument.presentationml.presentation',

  sit = 'application/x-stuffit',
  sxw = 'application/vnd.sun.xml.writer',

  iso = 'application/octet-stream',
  dmg = 'application/octet-stream',
  exe = 'application/octet-stream',
  dll = 'application/octet-stream',
  deb = 'application/octet-stream',
  otf = 'application/octet-stream',
  xpi = 'application/x-xpinstall',

  mp2 = 'audio/mpeg',
  mp3 = 'audio/mpeg',

  mpg = 'video/mpeg',
  mpeg = 'video/mpeg',
  flv = 'video/x-flv'
)


mime_type <- function(f) {
  f <- f[1]
  file_ext <- function (x) {
    pos <- regexpr('\\.([[:alnum:]]+)$', x)
    ifelse(pos > -1L, tolower(substring(x, pos + 1L)), '')
  }
  ext <- file_ext(f)
  ifelse(nchar(ext) > 1L && !is.null(.MIMEMAP[[ext]]), .MIMEMAP[[ext]], '')
}


base64_encode_file <- function(in_file, encoder) {

  file_size <- file.info(in_file)$size

  if (file_size <= 0) {
    warning(in_file, 'is empty!')
    return(in_file)
  }
  paste( 'data:', mime_type(in_file), ';base64,',
         encoder(readBin(in_file, 'raw', n = file_size)),
         sep = '')
}

# processes an HTML resource, given a regular expression that locates
# instances of that resource
process_html_res <- function(html, reg, processor) {
  html <- paste(html, collapse = "\n")
  m <- gregexpr(reg, html, perl = TRUE)
  if (m[[1]][1] != -1) {
    process_img_src <- function(img_src) {
      src <- sub(reg, '\\1', img_src)
      in_file <- utils::URLdecode(src)
      processor(img_src, src)
    }
    regmatches(html, m) <- list(unlist(lapply(regmatches(html, m)[[1]],
                                              process_img_src)))
  }

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
  process_images(html, base64_encode_img)
}

# get a base64 image encoder based on caTools
base64_image_encoder <- function() {
  function(data) base64_encode_images(data, caTools::base64encode)
}


