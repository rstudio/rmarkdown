# Currently last version of rstrap lives in rsformat package.
# TODO: Update here when source will change

dest <- "inst/rmd/h/rstrap"
if (!dir.exists(dest)) dir.create(dest)

rstrap_files <- xfun::with_ext("https://raw.githubusercontent.com/rstudio/rsformat/master/inst/rstrap/rstrap", c("css", "js"))
for (f in rstrap_files) xfun::in_dir(dest, xfun::download_file(f))

