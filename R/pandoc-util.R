

pandocTemplate <- function(file) {
  system.file(file.path("templates", file), package = "rmarkdown")
}

pandocOutputFile <- function(input, pandocFormat) {
  if (pandocFormat %in% c("latex", "beamer"))
    ext <- ".pdf"
  else if (pandocFormat %in% c("html", "html5", "revealjs"))
    ext <- ".html"
  else
    ext <- paste0(".", pandocFormat)
  output <- paste0(tools::file_path_sans_ext(input), ext)
  output
}

pandocExec <- function(pandoc, args, ...) {
  command <- paste(pandoc, paste(shQuote(args), collapse = " "))
  system(command, ...)
}

pandocPath <- function() {

  # check for versions of pandoc in rstudio and on the path
  rstudioPandocPath <- Sys.getenv("RSTUDIO_PANDOC")
  if (nzchar(rstudioPandocPath))
    rstudioPandocPath <- file.path(rstudioPandocPath, "pandoc")
  systemPandocPath = Sys.which("pandoc")

  # determine which one is more recent
  if (!nzchar(rstudioPandocPath) && !nzchar(systemPandocPath)) {
    stop("No version of pandoc was found on the path.", call.=FALSE)
  }
  else if (!nzchar(rstudioPandocPath))
    pandoc <- systemPandocPath
  else if (!nzchar(systemPandocPath))
    pandoc <- rstudioPandocPath
  else {
    rstudioVersion <- pandocVersion(rstudioPandocPath)
    systemVersion <- pandocVersion(systemPandocPath)
    if (rstudioVersion >= systemVersion)
      pandoc <- rstudioPandocPath
    else
      pandoc <- systemPandocPath
  }

  # verify the version
  version <- pandocVersion(pandoc)
  if (version < pandocRequiredVersion()) {
    stop("The rmarkdown package requires pandoc version ",
         as.character(pandocRequiredVersion()), " ",
         "or greater. You currently have version ", as.character(version), " ",
         "installed. Please update to a newer version.",
         call. = FALSE)
  }

  # return the path to pandoc
  pandoc
}

pandocVersion <- function(pandocPath) {
  versionInfo <- system(paste(shQuote(pandocPath), "--version"), intern = TRUE)
  version <- strsplit(versionInfo, "\n")[[1]][1]
  version <- strsplit(version, " ")[[1]][2]
  numeric_version(version)
}

pandocRequiredVersion <- function() {
  numeric_version("1.12.3")
}





