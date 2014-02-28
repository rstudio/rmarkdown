
sapply(list.files(pattern = glob2rx("*.Rmd")), rmarkdown::render)

