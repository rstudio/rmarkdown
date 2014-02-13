\name{ioslides_presentation}
\alias{ioslides_presentation}
\title{Convert to an ioslides Presentation}
\description{
  Format for converting from R Markdown to an \href{https://code.google.com/p/io-2012-slides/}{ioslides} presentation.
}
\usage{
ioslides_presentation(logo = NULL, incremental = FALSE, fig_width = 7.5,
  fig_height = 4.5, fig_retina = 2, fig_caption = FALSE, smart = TRUE,
  self_contained = TRUE, widescreen = FALSE, smaller = FALSE,
  transition = "default", mathjax = "default", css = NULL, includes = NULL,
  data_dir = NULL, pandoc_args = NULL)
}
\arguments{
  \item{logo}{Path to file that includes a logo for use in
  the presentation (should be square and at least 128x128)}

 \item{incremental}{\code{TRUE} to render slide bullets
  incrementally. Note that if you want to reverse the
  default incremental behavior for an individual bullet you
  can preceded it with \code{>}. For example: \emph{\code{>
  - Bullet Text}}}

  \item{fig_width}{Default width (in inches) for figures}

  \item{fig_height}{Default width (in inches) for figures}

  \item{fig_retina}{Scaling to perform for retina displays
  (defaults to 2, which currently works for all widely used
  retina displays). Note that this only takes effect if you
  are using knitr >= 1.5.21. Set to \code{NULL} to prevent
  retina scaling.}

  \item{fig_caption}{\code{TRUE} to render figures with
  captions}

  \item{smart}{Produce typographically correct output,
  converting straight quotes to curly quotes, --- to
  em-dashes, -- to en-dashes, and ... to ellipses.}

  \item{self_contained}{Produce a standalone HTML file with
  no external dependencies, using data: URIs to incorporate
  the contents of linked scripts, stylesheets, images, and
  videos. Note that if you specify "local" for
  \code{mathjax} then \code{self_contained} is
  automatically set to \code{FALSE}.}

  \item{widescreen}{Display presentation with wider
  dimensions.}

  \item{smaller}{Use smaller text on all slides. You can
  also enable this for individual slides by adding the
  \code{.smaller} attribute to the slide header (see
  \emph{Presentation Size} below for details).}

  \item{transition}{Speed of slide transitions. This can be "default",
  "slower", "faster", or a numeric value with a number of seconds (e.g.
  0.5)}

 \item{mathjax}{Include mathjax. The "default" option uses
  an https URL from the official MathJax CDN. The "local"
  option uses a local version of MathJax (which is copied
  into the output directory). You can pass an alternate URL
  or pass \code{NULL} to exclude MathJax entirely.}

  \item{css}{One or more css files to include}

  \item{includes}{Named list of additional content to
  include within the document (typically created using the
  \code{\link{includes}} function). If a \code{before_body}
  include is specified then it will replace the standard
  title slide entirely.}

  \item{data_dir}{Additional directory to resolve relatives
  paths of templates and included content against (the
  directory of the input file is used by default).}

  \item{pandoc_args}{Additional command line options to
  pass to pandoc}
}
\value{
R Markdown output format to pass to \code{\link{render}}
}
\section{Slide Basics}{

You can create a slide show broken up into sections by using the # and ##
heading tags (you can also create a new slide without a header using a
horizontal rule (\code{----------}). For example here's a simple slide show:

\preformatted{
---
title: "Habits"
author: John Doe
date: March 22, 2005
output: ioslides_presentation
---

# In the morning

## Getting up

- Turn off alarm
- Get out of bed

## Breakfast

- Eat eggs
- Drink coffee

# In the evening

## Dinner

- Eat spaghetti
- Drink wine

----------

![picture of spaghetti](images/spaghetti.jpg)

## Going to sleep

- Get in bed
- Count sheep

}

You can add a subtitle to a slide or section by including text after the
pipe (|) character. For example:

\preformatted{
## Getting up | What I like to do first thing
}

}



\section{Display Modes}{

The following single character keyboard shortcuts enable alternate
display modes:

\itemize{
  \item{\code{'f'}      }{enable fullscreen mode}
  \item{\code{'w'}      }{toggle widescreen mode}
  \item{\code{'o'}      }{enable overview mode}
  \item{\code{'h'}      }{enable code highlight mode}
  \item{\code{'p'}      }{show presenter notes}
}

Pressing \code{Esc} exits all of these modes. See the sections below
on \emph{Code Highlighting} and \emph{Presenter Mode} for additional
detail on those modes.

}

\section{Incremental Bullets}{

You can render bullets incrementally by adding the \code{incremental}
option:

\preformatted{
---
output:
  ioslides_presentation:
    incremental: true
---
}

If you want to render bullets incrementally for some slides but not
others you can use this syntax:

\preformatted{
> - Eat eggs
> - Drink coffee
}

}

\section{Presentation Size}{

You can display the presentation using a wider form factor using the
\code{widescreen} option. You can specify that smaller text be used with
the \code{smaller} option. For example:

\preformatted{
---
output:
  ioslides_presentation:
    widescreen: true
    smaller: true
---
}

You can also enable the \code{smaller} option on a slide-by-slide basis
by adding the \code{.smaller} attibute to the slide header:

\preformatted{
## Getting up {.smaller}
}

}

\section{Adding a Logo}{

You can add a logo to the presentation using the \code{logo} option (the
logo should be square and at least 128x128). For example:

\preformatted{
---
output:
  ioslides_presentation:
    logo: logo.png
---
}

A 128x128 version of the logo graphic will be added to the title slide and
an icon version of the logo will be included in the bottom-left footer of
each slide.
}

\section{Build Slides}{

Slides can also have a \code{.build} attribute that indicate that their
content should be displayed incrementally. For example:

\preformatted{
## Getting up {.build}
}

Slide attributes can be combined if you need to specify more than one,
for example:

\preformatted{
## Getting up {.smaller .build}
}
}

\section{Code Highlighting}{

Syntax highlighting in ioslides is doing using \href{https://code.google.com/p/google-code-prettify/}{Prettify}. It's also possible to select subsets of code
for special emphasis by adding a special "highlight" comment around the code.
For example:

\preformatted{
### highlight-begin
x <- 10
y <- x * 2
### highlight-end
}

When you press the \code{'h'} key the higlighted code will be brought
to the foreground.

}


\section{Tables}{

The ioslides template has an attractive default style for tables so you
shouldn't hestiate to add tables for presenting more complex sets of
information. Pandoc markdown supports several syntaxes for defining
tables which are described in the \href{http://johnmacfarlane.net/pandoc/demo/example9/pandocs-markdown.html}{pandoc markdown specification}.

}


\section{Advanced Layout}{

You can center content on a slide by adding the \code{.flexbox}
and \code{.vcenter} attributes to the slide title. For example:

\preformatted{
## Dinner {.flexbox .vcenter}
}

You can horizontally center content by enclosing it in a \code{div} tag
with class \code{centered}. For example:

\preformatted{
<div class="centered">
This text is centered.
</div>
}

You can do a two-column layout using the \code{columns-2} class.
For example:

\preformatted{
<div class="columns-2">
  ![Image](image.png)

  - Bullet 1
  - Bullet 2
  - Bullet 3
</div>
}

Note that content will flow accross the columns so if you want to
have an image on one side and text on the other you should make
sure that the image has sufficient height to force the text to
the other side of the slide.

}

\section{Text Color}{
You can color content using base color classes red, blue, green, yellow,
and gray (or variations of them e.g. red2, red3, blue2, blue3, etc.).
For example:

\preformatted{
<div class="red2">
This text is red
</div>
}
}

\section{Presenter Mode}{

A separate presenter window can also be opened (ideal for when you are
presenting on one screen but have another screen that's private to you).
The window stays in sync with the main presentation window and also
shows presenter notes and a thumbnail of the next slide. To enable
presenter mode add \code{?presentme=true} to the URL of the presentation,
for example:

\preformatted{
mypresentation.html?presentme=true
}

The presenter mode window will open and will always re-open with the
presentation until it's disabled with:

\preformatted{
mypresentation.html?presentme=false
}

To add presenter notes to a slide you include it within a "notes"
\code{div}. For example:

\preformatted{
<div class="notes">
This is my note.
- It can contain markdown
- like this list
</div>
}

}

\section{Printing and PDF Output}{

You can print an ioslides presentation from within browsers that have
good support for print CSS (i.e. as of this writing Google Chrome
has the best support). Printing maintains most of the visual styles
of the HTML version of the presentation.

To create a PDF version of a presentation you can use Print to PDF
from Google Chrome.

}

