# TO RUN IN A CLEAN SESSION
# install packages in a temp lib
withr::local_temp_libpaths(action = "replace")
.libPaths()
# In RStudio IDE, answer no if asked to restart R
install.packages("pak", repos = "https://r-lib.github.io/p/pak/dev/")
pak::local_install_dev_deps(".")

# remove bootstrap folder
bs_folder <- system.file("rmd/h/bootstrap", package = "rmarkdown", lib.loc = .libPaths()[1])
dir.exists(bs_folder)
unlink(bs_folder, recursive = TRUE)
dir.exists(bs_folder)

# check bslib is used and not internal bootstrap
library(rmarkdown,lib.loc = .libPaths()[1])
withr::local_dir("tests/manual/bslib-forced-mode/")
file <- "test.Rmd"
render(file, html_document(self_contained = FALSE))
render(file, html_document(self_contained = FALSE, toc = TRUE, toc_float = TRUE))
render(file, html_document(self_contained = FALSE, code_download = TRUE))
render(file, html_document(self_contained = FALSE, code_folding = "hide"))
render(file, html_document(self_contained = FALSE, theme = "cerulean"))

# reset the environment
withr::deferred_run()
