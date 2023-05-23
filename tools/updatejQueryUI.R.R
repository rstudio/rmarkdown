# This script aims to update and sync jquery-ui dependency with the one in shiny

rmd <- "inst/rmd/h/jqueryui/"

shiny <- "https://github.com/rstudio/shiny/archive/refs/heads/main.zip"

temp_zip <- tempfile(fileext = ".zip")
xfun::download_file(shiny, temp_zip, mode = "wb")

dir.create(temp_shiny <- tempfile("shiny"))

unzip(temp_zip, exdir = temp_shiny)

jqueryui <- file.path(temp_shiny, "shiny-main", "inst", "www", "shared", "jqueryui")
jquery_version <- file.path(temp_shiny, "shiny-main", "R", "version_jqueryui.R")

unlink(rmd, recursive = TRUE)

file.copy(jqueryui, dirname(rmd), overwrite = TRUE, recursive = TRUE)
file.copy(jquery_version, "R", overwrite = TRUE)

unlink(temp_shiny, recursive = TRUE)
