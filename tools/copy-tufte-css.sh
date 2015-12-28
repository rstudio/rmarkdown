#!/bin/bash

cp -r ../tufte-css/et-book ../tufte-css/tufte.css ../tufte-css/LICENSE inst/rmarkdown/templates/tufte_html/resources/
# only keep truetype fonts (.ttf)
find inst/rmarkdown/templates/tufte_html/resources/ -type f \( -name \*.woff -o -name \*.eot -o -name \*.svg \) -exec rm {} +
