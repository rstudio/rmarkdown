grab_clipboard <- function(filepath) {
  filepath <- xfun::normalize_path(filepath)
  if (xfun::is_macos()) {
    script <- paste0(
      "osascript -e \'
          set theFile to (open for access POSIX file \"",
      filepath, "\" with write permission)
      try
        write (the clipboard as \u00abclass PNGf\u00bb) to theFile
      end try
      close access theFile'"
    )
    system(script)
    # in mac os, if no image in clipboard, exec script will create a empty image
    if (file.size(filepath) == 0) file.remove(filepath)
  } else if (xfun::is_windows()) {
    script <- paste0(
      "powershell -sta \"\n",
      "Add-Type -AssemblyName System.Windows.Forms;\n",
      "if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {\n",
      "  [System.Drawing.Bitmap][System.Windows.Forms.Clipboard]::GetDataObject(
        ).getimage().Save('",
      paste0(filepath, "', [System.Drawing.Imaging.ImageFormat]::Png) \n"),
      "  }\""
    )
    system(script)
  } else {
    # Executing on Linux! -> use xclip
    tryCatch(
      targets <- tolower(
        system("xclip -selection clipboard -t TARGETS -o", intern = TRUE)
      ),
      error = function(e) {
        stop("Please install the required system dependency xclip", call. = FALSE)
      }
    ) # Validate xclip is installed and get targets from clipboard
    if (any(grepl(".*png$", targets))) {
      system(paste0("xclip -selection clipboard -t image/png -o > ", filepath))
    }
  }

  # in window, no image be create
  if (!file.exists(filepath) || file.size(filepath) == 0) {
    stop("Clipboard data is not an image.", call. = FALSE)
  }
}

guess_is_blogdown_post <- function() {
  # guess current rmd is a blogdown post?
  # Criteria:
  # - is a project and .Rproj have something like BuildType: Website
  # - filepath like content/**/**

  proj_root <- rstudioapi::getActiveProject()
  if (is.null(proj_root)) {
    return(FALSE)
  }

  proj_settings <- list.files(proj_root, pattern = ".Rproj", full.names = TRUE)
  currpath <- rstudioapi::getSourceEditorContext()$path
  if (any(grep("BuildType: Website", readLines(proj_settings)) > 0) &&
    basename(dirname(dirname(currpath))) == "content") {
    return(TRUE)
  }

  FALSE

}

generate_filepath <- function() {
  # return filepath and filepath_insert
  #   filepath: absolute path, to save image in clipboard
  #   filepath_insert: path in rmd code, ![](filepath_insert)
  #
  # for a blogdown post, filepath_insert is different from filepath
  # https://lcolladotor.github.io/2018/03/07/blogdown-insert-image-addin/#.XrZ9dxMzbjA
  #
  # for a generic rmd, filepath_insert is same with filepath,
  # while filepath_insert is relative path

  filename <- format(Sys.time(), "rmd-img-paste-%Y%m%d%H%M%s.png")
  currpath <- rstudioapi::getSourceEditorContext()$path
  if (!nchar(currpath)) {
    stop("Please save the file before pasting an image.", call. = FALSE)
  }

  is_post <- getOption("rmarkdown.is_blogdown_post", guess_is_blogdown_post())
  if (is_post) {
    proj_root <- rstudioapi::getActiveProject()
    post_files <- file.path(
      dirname(gsub(".*content/", "", currpath)),
      paste0(tools::file_path_sans_ext(basename(currpath)), "_files")
    )
    dir <- file.path(proj_root, "static", post_files)
    baseurl <- getOption("rmarkdown.blogdown_baseurl", "")
    dir_insert <- file.path(baseurl, post_files)
  } else {
    dir_insert <- getOption("rmarkdown.paste_image_dir", ".assets")
    dir <- file.path(dirname(currpath), dir_insert)
  }

  if (!file.exists(dir)) dir.create(dir, recursive = TRUE)
  list(
    filepath = file.path(dir, filename),
    filepath_insert = file.path(dir_insert, filename)
  )
}

insert_image_from_clipboard_addin <- function() {

  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop("rstudioapi needed for paste image into rmd. Please install it.",
         call. = FALSE)
  }

  doc_id <- rstudioapi::getSourceEditorContext()$id
  if (doc_id %in% c("#console", "#terminal")) {
    stop("You can`t insert an image in the console nor in the terminal.
         Please select a line in the source editor.", call. = FALSE)
  }
  res <- generate_filepath()
  grab_clipboard(res$filepath)

  position <- rstudioapi::getSourceEditorContext()$selection[[1]]$range$start
  func <- function(filepath) paste0("![](", filepath, ")")
  rstudioapi::insertText(position, func(res$filepath_insert), id = doc_id)
}
