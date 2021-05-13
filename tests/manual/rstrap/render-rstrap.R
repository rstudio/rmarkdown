# TO RUN IN A CLEAN SESSION
# install packages in a temp lib
withr::local_temp_libpaths(action = "replace")
.libPaths()
install.packages("pak", repos = "https://r-lib.github.io/p/pak/dev/")
pak::local_install_dev_deps(".")

# remove boostrap folder
bs_folder <- system.file("rmd/h/bootstrap", package = "rmarkdown", lib.loc = .libPaths()[1])
dir.exists(bs_folder)
unlink(bs_folder, recursive = TRUE)
dir.exists(bs_folder)

# check rstrap is used and not bootstrap
library(rmarkdown,lib.loc = .libPaths()[1])
withr::local_dir("tests/manual/rstrap/")
render("rstrap.Rmd", html_document(self_contained = FALSE))
render("rstrap.Rmd", html_document(self_contained = FALSE, toc = TRUE, toc_float = TRUE))
render("rstrap.Rmd", html_document(self_contained = FALSE, code_download = TRUE))
render("rstrap.Rmd", html_document(self_contained = FALSE, code_folding = "hide"))

# reset the environment
withr::deferred_run()
