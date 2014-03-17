\name{draft}
\alias{draft}
\title{Create a new document based on a template}
\usage{
draft(file, template, package = NULL, edit = TRUE)
}
\arguments{
  \item{file}{File name for the draft}

  \item{template}{Template to use as the basis for the
  draft. This is either the full path to a template
  directory or the name of a template directory within the
  \code{rmarkdown/templates} directory of a package.}

  \item{package}{(Optional) Name of package where the
  template is located.}

  \item{edit}{\code{TRUE} to edit the template immediately}
}
\value{
The file name of the new document (invisibly)
}
\description{
Create (and optionally edit) a draft of an R Markdown
document based on a template.
}
\details{
The \code{draft} function creates new R Markdown documents
based on templates that are either located on the
filesystem or within an R package. A new directory bearing
the same name as the R Markdown file is created then the
template and any supporting files are copied into the
directory.
}
\note{
An R Markdown template consists of a directory that
contains a description of the template, a skeleton Rmd file
used as the basis for new documents, and optionally
additional supporting files that are provided along with
the skeleton (e.g. a logo graphic).

If the template directory is contained within a package
then it should be located at
\code{inst/rmarkdown/templates}. For example, a package
named \pkg{pubtools} that wanted to provide a template
named \code{quarterly_report} would need to provide the
following files within the
\code{pubtools/inst/rmarkdown/templates} directory:

\code{quarterly_report/template.yaml} \cr
\code{quarterly_report/skeleton/template.Rmd} \cr

The \code{template.yaml} file should include the two fields
\code{name} and \code{description}. The
\code{skeleton/template.Rmd} file should include the
initial contents you want for files created from this
template.  Additional files can be added to the
\code{skeleton} directory, for example:

\code{skeleton/logo.png} \cr

These files will automatically be copied to the directory
containing the new R Markdown draft.
}
\examples{
\dontrun{

rmarkdown::draft("Q4Report.Rmd",
                 template="/opt/rmd/templates/quarterly_report")

rmarkdown::draft("Q4Report.Rmd",
                 template="quarterly_report", package="pubtools")
}
}

