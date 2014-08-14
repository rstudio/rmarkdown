\NeedsTeXFormat{LaTeX2e}[1994/06/01]

\ProvidesClass{tufte-handout}[2009/12/11 v3.5.0 Tufte-handout class]

%%
% Declare we're tufte-handout
\newcommand{\@tufte@class}{article}% the base LaTeX class (defaults to the article/handout style)
\newcommand{\@tufte@pkgname}{tufte-handout}% the name of the package (defaults to tufte-handout)

%%
% Load the common style elements
\input{tufte-common.def}


%%
% Set up any handout-specific stuff now

%%
% Abstract

% TODO The abstract should be printed on its own page of the `titlepage'
% option was specified.
\renewenvironment{abstract}
  {\begin{quotation}\if@tufte@sfsidenotes\begin{sffamily}\fi}
  {\if@tufte@sfsidenotes\end{sffamily}\fi\end{quotation}}


%%
% If there is a `tufte-handout-local.tex' file, load it.

\IfFileExists{tufte-handout-local.tex}
  {\input{tufte-handout-local}
   \TufteInfoNL{Loading tufte-handout-local.tex}}
  {}


%%
% End of file
\endinput
