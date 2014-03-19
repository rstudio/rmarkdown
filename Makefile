
HTML_FILES := $(patsubst %.Rmd, %.html ,$(wildcard *.Rmd)) \
              $(patsubst %.md, %.html ,$(wildcard *.md))

all: clean html


html: $(HTML_FILES)

%.html: %.Rmd
	R --vanilla --slave -e "rmarkdown::render('$<')"

%.html: %.md
	R --vanilla --slave -e "rmarkdown::render('$<')"

.PHONY: clean
clean:
	$(RM) $(HTML_FILES)

