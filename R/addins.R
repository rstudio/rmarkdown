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

  if (!file.exists(filepath)) {
    stop("Clipboard data is not an image.", call. = FALSE)
  }
}

is_blogdown_post <- function() {
  # is current rmd a blogdown post?
  # criteria:
  # - options(rmarkdown.is_blogdown_post = TRUE)
  # - is a project and .Rproj have attr like BuildType: Website
  # - filepath like content/**/your_post_name.rmd

  if (getOption("rmarkdown.is_blogdown_post", FALSE)) {
    return(TRUE)
  }
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

generate_default_filepath <- function() {
  # return default filepath (absolute):
  # - rmd: .assets/rmd-img-paste-%Y%m%d%H%M%s.png
  # - post: static/../your_post_name_files/rmd-img-paste-%Y%m%d%H%M%s.png

  currpath <- xfun::normalize_path(rstudioapi::getSourceEditorContext()$path)
  if (currpath == "") {
    stop("Please save the file before pasting an image.", call. = FALSE)
  }

  filename <- format(Sys.time(), "rmd-img-paste-%Y%m%d%H%M%s.png")
  dir <- file.path(
    dirname(currpath),
    getOption("rmarkdown.paste_image_dir", ".assets")
  )

  if (is_blogdown_post()) {
    proj_root <- rstudioapi::getActiveProject()
    post_files <- file.path(
      gsub(".*content/", "", dirname(currpath)),
      paste0(tools::file_path_sans_ext(basename(currpath)), "_files")
    )
    dir <- file.path(proj_root, "static", post_files)
  }

  # by default, this will create a subfolder if not exist
  # override by options(rmarkdown.create_sub_dir = FALSE)
  if (!file.exists(dir) && getOption("rmarkdown.create_sub_dir", TRUE)) {
    file.create(dir, recursive = TRUE)
  }
  file.path(dir, filename)
}

dialog_set_filename <- function(default_filepath) {
  filepath <- rstudioapi::selectFile(
    caption = "Save File",
    label = "Save",
    path = default_filepath,
    existing = FALSE
  )
  filepath <- xfun::normalize_path(filepath)
  proj_root <- rstudioapi::getActiveProject()
  if (is_blogdown_post() &&
    (!startsWith(filepath, file.path(proj_root, "static")))) {
    stop("for blogdown post, image should in static/", call. = FALSE)
  }
  filepath
}

insert_image_from_clipboard_addin <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop("rstudioapi needed for paste image into rmd. Please install it.",
      call. = FALSE
    )
  }
  doc_id <- rstudioapi::getSourceEditorContext()$id
  if (doc_id %in% c("#console", "#terminal")) {
    stop("You can`t insert an image in the console nor in the terminal.
         Please select a line in the source editor.", call. = FALSE)
  }

  filepath <- generate_default_filepath()
  if (getOption("rmarkdown.show_dialog", TRUE)) {
    filepath <- try(dialog_set_filename(filepath), silent = TRUE)
    if ('try-error' %in% class(filepath)) {
      message('cancel insert image')
      return()
    }
  }
  grab_clipboard(filepath)
  position <- rstudioapi::getSourceEditorContext()$selection[[1]]$range$start
  func <- function(filepath) paste0("![](", filepath, ")")
  if (is_blogdown_post()) {
    baseurl <- getOption("rmarkdown.blogdown_baseurl", "")
    filepath <- file.path(baseurl, gsub(".*static/", "", filepath))
  }
  rstudioapi::insertText(position, func(filepath), id = doc_id)
}
