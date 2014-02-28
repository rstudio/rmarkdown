
HTML_FILES := $(patsubst %.Rmd, %.html ,$(wildcard *.Rmd)) \
              $(patsubst %.md, %.html ,$(wildcard *.md))

all: $(HTML_FILES)

%.html: %.Rmd
	R --vanilla --slave -e "rmarkdown::render('$<')"

%.html: %.md
	R --vanilla --slave -e "rmarkdown::render('$<')"
