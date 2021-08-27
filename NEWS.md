rmarkdown 2.11
================================================================================

- Relative paths in parent directories in the `css` argument of `html_document()` were incorrectly normalized to absolute paths by #2095 in v2.8. Now relative paths in parent directories will no longer be converted to absolute paths (thanks, @daijiang, yihui/xaringan#331).

- It is possible to specify the version of jQuery via a global option now, e.g., `options(rmarkdown.jquery.version = 2)` (note that the default major version is `3`). This is mainly for advanced users and developers to test different versions of jQuery.

- `pandoc_citeproc_convert()` now handles correctly bib file containing specific UTF-8 characters on non default UTF-8 systems like Windows (thanks, @mitchelloharawild, #2195).

- Shiny prerendered documents are now pre-rendered in a child environment to avoid allowing the results of static code chunks to exist in the Shiny app environment (@gadenbuie, #2203).

- The previously unexported function `convert_ipynb()` is exported now (thanks, @acircleda).


rmarkdown 2.10
================================================================================

- `md_document()` will now handle correctly `preserve_yaml` value for all variants and all pandoc versions (#2190). 
  * with `preserve_yaml = TRUE`, markdown output will keep the YAML metadata block from the Rmd file.
  * with `preserve_yaml = FALSE`, markdown output will have no YAML metadata block.
  
  This fixes a breaking change in Pandoc 2.13 regarding `gfm`, `commonmark` and `commonmark_x` which now supports `yaml_metadata_block` by default (#2118).  

- New supported syntax for Shiny prerendered documents: you can now  use `server: shiny` or `server: type: shiny`.

- Ability to inject additional functions into Shiny prerendered server scope using the "server-extras" context.

- Fixed the syntax highlighting issue with R's pipe operator `|>` (thanks, @edzer, rstudio/bookdown#1157).


rmarkdown 2.9
================================================================================

- Fix a regression in version 2.8 when a url is used in `css` argument (thanks, @vnijs, #2163).

- All HTML dependencies are now correctly supported, included those with only an `href` component but not `file` component in their `src` attribute. Previously, **rmarkdown** would throw the error `'path for html_dependency not provided'` when rendering documents containing HTML dependencies with `href` components (thanks, @crazycapivara, @matthewstrasiotto, #1805, #1948, #2151).

- Fix an error thrown with output format using a `file_scope` function (like in **bookdown**) (thanks, @rfaelens, #2149).

- Fix an issue with `copy_ressource = TRUE` in `html_document_base` where very long HTML documents were truncated during post processing (thanks, @oliviermeslin, #2145).

- When `run()`-ing a `runtime: shiny` document, an extra temp folder will be used in the output path. With the extra temp random folder in the path, predictable output file names may be used. (#2137)

- When `run()`-ing a `runtime: shiny` document with a `{bslib}` theme, the global theme value wasn't being restored properly. (#2160)

- Floating ToC in `html_document` can now hide headings with unnumbered and unlisted classes (thanks, @atusy, #1993).

- Fix prefix handling in R Markdown website's navbar for Fontawesome V5 and compatibility with V4. For icon only available in V5, the full prefix + name should be use, especially with new `fab` prefix (e.g. `fab fa-r-project`). If no prefix is used (e.g `fa-home` instead of `fas fa-home`), the `fa` prefix will be added for V4 compatibility as it has been deprecated in V5. We advice to use the full prefix + name for icons following Fontawesome documentation. (#1994)

- `rmarkdown::site_generator()` can hang session waiting for input when the `site` field is not found in the YAML frontmatter of `index.Rmd` (thanks, @kevinushey @mirh, #2043).


rmarkdown 2.8
================================================================================

- Fix a issue with Pandoc 2.5 and `latex-div.lua` - documents can now be rendered as expected without error (thanks, @davidwales, #2121).

- Fix an issue with styling and code folding button behavior when default is `code-folding: show`. The Button can now be correctly style according to state as `aria-expanded` attributes is correctly updated. Also, new classes has been added on the button to allow styling during transition: `btn-collapsing` and `btn-expanding` are respectively applied during transition Show to Hide and Hide to Show. (This follow [Bootstrap behavior for the collapsible block](https://getbootstrap.com/docs/3.4/javascript/#collapse)) (thanks, @steveharoz, #2085).

- Fix an issue with `citation_package` having no effect when using `.md` file as input to `render()` with latex and PDF output formats (thanks, @andrewheiss, #2113).

- A new internal option `rmarkdown.knit.ext` has been added to control the extension of the intermediary knit output during a rendering. It defaults to `md` to produce `*.knit.md`. Only useful for very advanced usage (#2098).

- `render()` won't produce any `*.utf8.md` intermediary file anymore. This was a leftover from previous versions of **rmarkdown**. Since **knitr** 1.24 and **rmarkdown** 2.0, only UTF-8 input files are allowed. (#2098).

- Fix an `Invalid cross-device link` error when `tempdir()` is used for `intermediates_dir` in `render()` (thanks, @gorgitko, #2096).

- Fix a regression in HTML default template with floating toc incorrectly placed on small size window (thanks, @grimbough, #2071)

- Provided a `runtime: shiny` fix for output formats that pass a modified `bslib::bs_theme()` object to `html_document_base()`'s `theme` (thanks, @cpsievert, #2049).

- Rendering using `runtime: shiny_prerendered` or `runtime: shinyrmd` will now produce valid HTML by not inserting anymore the full document as body in the resulting shiny apps (thanks, @dakep, #1912). Header content usually containing html dependencies will be inserted in the HTML document at the end of the head before `</head>`, unless the rendered HTML contains `<!-- HEAD_CONTENT -->` special comment (see `htmltools::renderDocument()`). A new Pandoc variable is set in for shiny prerendered document to allow conditional insertion of such content in the the HTML template using `$if(shiny-prerendered)$`. This has been done in all HTML template in this package. Users of custom template should make this change to provide support for this runtime. See **rmarkdown** default template (`default.html`) for an example (#2064).

- Added `tectonic` as a supported LaTeX engine for generating PDF output (thanks, @dpryan79, #2078). You can specify to use this by adding `engine: "tectonic"` to your output format in YAML, such as `pdf_document`.

- When no `output_format` is provided in any way but an `output_file` is provided in `render()`, the default format will be determined based on the extension: `"pdf_document"` for `.pdf`, or `"word_document"` for `.docx`. Otherwise, it will be `"html_document"` as previous version (thanks, @pearsonca, #1569).

- Added a new global option `rmarkdown.render.message`. When set `FALSE`, `render()` will not output the message starting by `Output created: ` allowing RStudio IDE to open a preview of the document. This is useful for package developers that would need to emit there own output message for there custom format. See `?render_site` for more info on this special message (#2092).

- Internal changes regarding Lua filters. They have now an explicit Pandoc version minimal requirement: A filter will be skipped with a warning printed by the Lua filter if this requirement is not met. For now, all filters work for Pandoc 2.1 and above (thanks, @atusy, #2088). There is also now a new mechanism to have a share Lua filter script loadable by other Lua files: `render()` will set the `RMARKDOWN_LUA_SHARED` env var to the path of Lua filter `shared.lua` so that other filters can access functions defined in it using `dofile(os.getenv 'RMARKDOWN_LUA_SHARED')`. This is for internal usage only to avoid duplication (thanks, @tarleb, #2103).

- `html_document_base` gains a `css` argument, to which `html_document`'s `css` argument is now passed. This also fix an issue when `.sass` or `.scss` files are used with this `css` argument when `self_contained: FALSE`. Moreover, **sass** caching mechanism can now be used when passing `.sass` or `.scss` files to the `css` argument (thanks, @cpsievert, #2095).

- The `fig_crop` option of PDF document formats (such as `pdf_document` and `beamer_presentation`) supports the value `"auto"` now, which means `fig_crop = TRUE` when figure cropping tools `pdfcrop` and `ghostscript` are available.

- The default value of the `fig_crop` option of PDF output formats has been changed from `TRUE` to `"auto"` (#2077).

- `rmarkdown::tufte_handout` has been deprecated and will be removed in the future from this package. It has been moved to the **tufte** package since **rmarkdown** 0.9.5 (released on 2016-02-22). Please use `tufte::tufte_handout` instead.


rmarkdown 2.7
================================================================================

- `html_document` (and `html_document_base`)'s `theme` parameter now understands `bslib::bs_theme()` objects/arguments, meaning that one may opt-into Bootstrap 4 and more easily create custom themes. For examples, see <https://github.com/rstudio/rmarkdown/pull/1706>, and for context, see <https://rstudio.github.io/bslib/> (thanks, @cpsievert, #1706).

- Files with `.scss`/`.sass` extension (i.e., Sass files) provided to `html_document`'s `css` parameter are now compiled to CSS using the `{sass}` package. Also, if `theme` is a `{bslib}` object, these Sass files may utilize Sass code inside the `theme` (thanks, @cpsievert, #1706).

- Fix an issue with line numbering in code chunks when `.numberlines` with Pandoc's highlighting (thanks, @aosavi, #1876).

- Fix an issue with shiny runtime and `global.R` (thanks, @liaojiahui-r, rstudio/flexdashboard#298).

- Accept `latex="{options}"`, `latex=1`, or `latex=true` for Latex Divs.

- Add `output_format_filter` function to `default_site_generator()`. Enables custom site generators to customize or even entirely replace the output format right before rendering of each page.

- Automatically exclude **renv** directory for `render_site()` (thanks, @jmbuhr, #1996)

- Do not force `options(htmltools.preserve.raw = TRUE)` when this option has been set, otherwise it is impossible for other packages to turn this option off, e.g., yihui/xaringan#293.

- `knitr_options_pdf()` will now throw a warning when `fig_crop = TRUE` but is disabled because required tools `pdfcrop` and/or `ghostscript` are missing (thanks, @netique, #2016).

- Eliminated the unnecessary padding in code blocks in the `html_document` output with Bootstrap 4 themes (thanks, @atusy, #2019).

- `github_document()` will produce a working TOC even if some headers start with number (#2039).

- Fix an issue with `knit_print.data.frame`. The `...` arguments are no more passed to `print()` to avoid passing `knit_print()` arguments `options` and `encoding` to custom `print()` methods (#2047).


rmarkdown 2.6
================================================================================

- Encoding is correctly handled now in `html_vignette` when checking for identical title and vignette index entry (thanks, @py-b, #1978).

- `clean_site()` now default to `preview = TRUE` and will no more remove files without notice. This change will affect the "Clean All" button in the "Build" pane for website project. `clean_site(preview = FALSE)` must be run to effectively remove files (#1973).

- The intermediate `.tex` file is now correctly deleted if `keep_tex = FALSE` when the R Markdown document is not rendered from the working directory (thanks, @vqv, #1308).

- Fix a bug causing certain resources files to be deleted as intermediate files when `intermediates_dir` is the same as the input (thanks, @bellma-lilly, #1248). 

- Fix issues with `anchor_sections = TRUE` and **learnr** (thanks, @gadenbuie, #1938).

- Enable use of `server.R` and `global.R` alongside `runtime: shinyrmd` documents.

- `pkg_file_lua()` now works with `devtools::load_all()` and **testthat** when used in other packages. 

- Fix `pandoc_convert(citeproc = TRUE)` not supressing the `--natbib` or `--biblatex` options (thanks, @atusy, #1932).

- `pandoc-citeproc` is now activated if a `bibliography` field is defined in another YAML block instead of the first YAML block (thanks, @bwiernik, #1364).

- Specify that `htmltools::htmlPreserve()` should use the pandoc raw attribute 
  rather than preservation tokens when pandoc >= v2.0. Note that this option will
  have the intended effect only for versions of htmltools >= 0.5.0.9003.

- `anchor_sections` in `html_documents()` now defaults to `FALSE`. It was introduced in previous version with a default to `TRUE`, but it is reverted now after hearing feedbacks from the community (thank you!). The `#` is still used as the character for the anchor but you can easily change that using CSS rules. Examples have been added to the help page `?html_document`.

- Using Pandoc's default for `--email-obfuscation` now. Previously, it was set to `none` explicitly, which is the default for Pandoc 1.17.2+ anyway. Only users with a Pandoc version before 1.17.2 may see a change in the content of the html source file produced if the document contains email addresses. This change allows to pass the Pandoc's command line flag if you want to set it to another value  (thanks,@seankross, #1969). 

  ````yaml
  output:
    html_document: 
      pandoc_args: ["--email-obfuscation", "javascript"]
  ````

  See [Pandoc's manual](https://pandoc.org/MANUAL.html#option--email-obfuscation) for the meaning of this option. 
  
- Fix Fontawesome 5 icons in navbar by correctly handling new prefix as `fa` has been deprecated in favor of `fas` or `fab` (#1967)


rmarkdown 2.5
================================================================================

- Tables without header rows (wich can be possible in Pandoc's [simple table](https://pandoc.org/MANUAL.html#extension-simple_tables)) are now formatted correctly when using `html_document()` format (thanks, @fkohrt, #1893).

- `html_document()` gains the `anchor_sections` argument, which is `TRUE` by default, so that readers can get links to section headers easily---when you mouse over a section header, you will see a hash symbol `#` at the end of the header, which contains the anchor link to this header. You can click on this link and get the URL in the addres bar of your web browser, or right-click on it and copy the URL from the context menu. The hash symbol is defined by the CSS rule `a.anchor-section::before {content: '#';}`. You can customize it by overriding this rule (e.g., via the `css` argument of `html_document`) and use any other symbols or icons, e.g., `content: "\02AD8;"` (thanks, @atusy, #1884).

- `pkg_file_lua()` should have thrown an error if the expected Lua file does not exist.

- Provide `files_dir` and `intermediates_dir` as attributes on return from `render()` when `run_pandoc = FALSE`.

- Supports new Pandoc 2.11 `--citeproc` flags usage instead of `pandoc-citeproc` external filter. `pandoc_convert()` and `pandoc_citeproc_convert()` will now use the correct flags according to the Pandoc version used. The logic is exported in `pandoc_citeproc_args()`. See [Pandoc release note](https://github.com/jgm/pandoc/releases/tag/2.11) for more information about the new `citeproc` processing (#1916).

- Fixed the code highlighting when code block is hidden. Previous version introduced a regression where non default code highlighting was still shown when `code_folding` is activated and code block is hidden (thanks, @matthewcarlucci, #1921).

- The minimal required version for the **xfun** package (v0.15) has been specified for R Markdown Notebooks to work properly (thanks, @jmcphers, #1923).

- Fixed a bug in `convert_ipynb()` when the language is not specified in the `.ipynb` file (thanks, @acca3003, #1925).

- Introduce `runtime: shinyrmd` as a more user friendly alias for `runtime: shiny_prerendered`.


rmarkdown 2.4
================================================================================

- Lua filters handling has been improved internally with some user-facing changes (#1899):
    - New exported function `pandoc_lua_filter_args()` to return the Pandoc command-line argument to add a Lua filter.
    - New argument `lua_filters` in `pandoc_options()` to pass the Lua filter paths to use with a format. This allow output format authors to add filters for a custom format using the `pandoc` argument of `output_format()` and to get filters from a format using `fmt$pandoc$lua_filters`.
    - The Lua filters of an output format are now passed to Pandoc in `render()`. By default, they are passed to Pandoc before any other format-defined or user-defined Pandoc arguments (usually via the `pandoc_args` option of an output format). This ensures that filters of an output format are executed first. To change the default, you need to deal with it in a custom format (i.e., modify the elements in `fmt$pandoc$lua_filters`, such as reordering them).
    - New exported function `pkg_file_lua()` to get the full system path of a Lua filter included in a package source within the `inst/rmarkdown/lua` folder (thanks, @atusy, #1903).

- Fixed the path separators for the `css` parameter in YAML frontmatter for HTML output files under Windows. Previously, forward slashes in `css` paths were converted to backslashes (thanks, @jonathan-g, #1862).

- Since **rmarkdown** 1.16, Pandoc's fenced `Div`'s are converted to LaTeX environments when the output format is LaTeX, e.g., `::: {.center data-latex=""}` is converted to `\begin{center}`. The attribute `data-latex` of the `Div` was mandatory, even if it is empty. In **rmarkdown** 2.2, we silently drop this requirement, which means `::: {.center}` is converted to `\begin{center}`. This turns out to be a bad idea, because users have no control over which Div's to be converted to LaTeX environments. Previously, they could opt-in by the `data-latex` attribute, but with **rmarkdown** 2.3, all Div's are converted to LaTeX environments unconditionally. What's more, this change led to bugs like https://stackoverflow.com/q/62340425/559676 and https://github.com/rstudio/bookdown/issues/883. Therefore the `data-latex` attribute became mandatory again in this version. If the LaTeX environment does not need arguments, you may use `data-latex=""`.

- The two Lua fitlers `pagebreak.lua` and `latex-div.lua` (introduced in **rmarkdown** 1.16) are also applied to the output format `beamer_presentation` now (thanks, @XiangyunHuang, #1815).

- When customizing formats with the `output_format` function, `pre_knit`, `opts_hooks`, and `knit_hooks` can now refer to `rmarkdown::metadata`. Previously, `rmarkdown::metadata` returned `list()` in these functions (thanks, @atusy, #1855).

- `rmarkdown::find_external_resources()` now discovers external template files. This in turn fixes the rendering issue of `html_document` with the `shiny` runtime and `intermediate_dir` set (thanks, @atusy, @cderv, #1865).

- Added the `number_sections` argument to following formats: `github_document`, `ioslides_presentation`, `md_document`, `odt_document`, `powerpoint_presentation`, `rtf_document`, `slidy_presentation`, `word_document`. These are powered by a Lua filter and requires Pandoc > 2.0. It will silently have no effect has before with previous pandoc version (thanks @atusy 1893).  Pandoc >= 2.10.1 adds `--number-sections` for docx format, and thus `word_document` prefers the native feature to the Lua filter (thanks, @jooyoungseo, #1869).

- For the output format `pdf_document`, the option `fig_crop` will not be enabled unless both the programs `pdfcrop` and `ghostscript` are found (thanks, @dalupus, yihui/knitr#954).

- Fixed a bug that a chunk with a class `fold-hide` hides the rest of the chunks even the output format setting `html_document(code_folding = "show")` (thanks, @atusy, #1906).

- Updated documentation for `render()` to make it clearer how options are set for the `output_format` parameter (thanks, @jonathan-g, #1907 and rstudio/bookdown#930).

- Ported some CSS styles (e.g., underlines, small caps, and multi-column layouts) from the latest Pandoc's HTML template into **rmarkdown**s HTML templates (thanks, @atusy, #1878, #1908).


rmarkdown 2.3
================================================================================

- Addressed an accessibility issue in highlighted code blocks of HTML output for screen reader users: screen readers no longer read out an unnecessary code line id values (thanks, @jooyoungseo and @atusy, #1833).

- Added `file_scope` option to output format definition. This enables handling of duplicate numeric footnote identifiers (e.g. across bookdown chapters) via the pandoc `--file-scope` option (#1837).

- Added the customizable `lang` attribute to `ioslides_presentation` output (thanks, @jooyoungseo, #1841).

- Added `publish_site()` function for "one-button" publishing of R Markdown websites.

- When the `df_print` option is `kable` and the output format is not HTML, `<div class="kable-table">` is no longer added to the `kable()` output, because recent versions of Pandoc will convert the `div` to a LaTeX environment when the output format is LaTeX (thanks, Laurens, https://stackoverflow.com/q/62340425/559676).

- `html_vignette()` only warns against differences in the vignette title and the vignette index entry for R >= 3.6.0 (thanks, @krlmlr, #1832).

- `html_document()` can apply `code_folding` on any chunk engines if the `foldable` class is added to a source code block via the chunk options (`class.source` or `attr.source`). You may apply this feature to all the source code blocks, for example, by setting `knitr::opts_chunk$set(class.source = "foldable")` at the beginning of your document (thanks, @atusy, #1835).


rmarkdown 2.2
================================================================================

- Exported the internal function `find_pandoc()`, and also added two arguments, `dir` and `version`, so that users can provide a custom directory under which this function may find Pandoc, as well as an expected version of Pandoc to be found (thanks, @connorp, #1785).

- `pandoc_metadata_arg()` is a new Pandoc helper function to generate `--metadata` argument for Pandoc command line (thanks, @cderv, #1789).

- The output format `html_vignette()` now warns against differences in the vignette title specified in the `title` field in the YAML metadata and the one specified inside `\VignetteIndexEntry{}`. Normally they are expected to be identical (#1789).

- Fixed a bug with encoding when rendering `html_notebook` containing HTML widgets (thanks, @cderv, #1799).

- TOC title can now be specified for `html_document` via the top-level option `toc-title` in the YAML frontmatter (thanks, @atusy, #1771).

- Floating TOC can now distinguish upper/lower-cases (thanks, @atusy, #1783).

- When `code_folding='show'` for the output format `html_document`, code blocks can be individually hidden initially by specifying the chunk option `class.source='fold-hide'` (thanks, @atusy, #1798).

- For LaTeX/PDF output formats `pdf_document`, `beamer_presentation`, and `context_document`, the argument `citation_package = 'none'` was deprecated, and `citation_package = 'default'` should be used instead if citations are to be processed by `pandoc-citeproc` (thanks, @njbart, rstudio/bookdown#754).

- `output_format()` can now inherit `keep_md` and `clean_supporting` from `base_format` when `NULL` is passed to these arguments. Previously, you must explicitly specify `keep_md` and/or `clean_supporting` as `TRUE` or `FALSE` in `output_format()` since they could not inherit the corresponding options of `base_format`. This behavior was not consisent with other arguments of `output_format()` (thanks, @atusy, #1823).

- The `smart` argument of most output formats has been removed, because Pandoc's `smart` extension is enabled by default, and setting `smart: false` for an output format did not really have any effect (which could be considered a bug, but we want to get rid of this option since it existed only for a historical reason for Pandoc 1.x, and Pandoc 2.x has been released for more than two years). If you want to disable the `smart` extension, you can use the option `md_extensions: -smart` of the output format (thanks, @atusy, #1774).

- `pdf_document()` should not specify the `geometry` variable when the `documentclass` variable is passed to Pandoc (thanks, @jpcirrus, #1782).

- `render()` now respects the YAML metadata in the R script when rendering the script with Pandoc 2.8 or later (thanks, @nsoranzo #1740, @cderv #1741).

- For `pandoc_convert()`, when the argument `to = 'pdf'`, it will be changed to `'latex'` internally (thanks, @JohannesFriedrich, #1802).

- `render(run_pandoc = FALSE)` no longer cleans up the Markdown file (typically knitted from Rmd) (thanks, @BrianDiggs, #1812).


rmarkdown 2.1
================================================================================

- Added the returned output from `shiny::runApp()` within `rmarkdown::run()` (thanks, @schloerke, #1760).

- YAML header is now correctly parsed in `html_notebook`'s intermediate `.knit.md` file so that features like adding bibliography works again (thanks, @everdark, @cderv, #1747).

- `ioslides_presentation` template no longer generates an empty `<h2>` tag when `subtitle` is not specified in YAML (thanks, @jooyoungseo #1735, @cgrudz #1663).

- No longer center the `#header` element in the `html_vignette()` output (thanks, @EmilHvitfeldt, #1742).

- Ensure the `tempdir()` exists (via `tempdir(TRUE)`) when writing HTML dependencies to a temporary file, because this directory might be erased by accident (thanks, Kurt Hornik, and also @karawoo #1743).

- Added the `slide_level` argument to `slidy_presentation()` (https://stackoverflow.com/q/59157211/559676).

- Removed the jQuery dependency in `html_document_base()` (#1723). To avoid bugs like #1723, Pandoc 2.8 users have to upgrade to Pandoc 2.9+.

- For `pdf_document`, horizontal rules generated by Pandoc (before v2.8) stopped working in recent versions of TeX Live, and the same fix as the one to https://github.com/jgm/pandoc/issues/5801 (i.e., hard-code `\linethickness` to `0.5pt`) was applied in **rmarkdown** (thanks, @cderv, https://stackoverflow.com/a/58646915/559676).


rmarkdown 2.0
================================================================================

- For the output format `pdf_document()`, we no longer adjust the vertical spacing of the title area. This means there will be a larger spacing above the document title in PDF. If you prefers the old (smaller) spacing, please download https://github.com/rstudio/rmarkdown/blob/f6961af/inst/rmd/latex/compact-title.tex and include it to the preamble via the `includes` option of `pdf_document`. However, please note that this means you won't be able to have multiple authors in the `author` field of the YAML frontmatter, unless you use a custom LaTeX template. With the default LaTeX template, you will run into the error in #1716. Besides, the `compact-title` option in YAML is no longer supported.

- R code in the `header-includes` field in the YAML frontmatter stopped working in the previous version of **rmarkdown**. The code should be evaluated before passing to Pandoc (thanks, @mcol #1709, @cderv #1710).

- The `encoding` argument is no longer passed to the `intermediates_generator` of R Markdown output formats. The `intermediates_generator` function can only accept arguments `input_file` and `intermediates_dir` now (see `?rmarkdown::output_format`). This is a breaking change to developers. If you are an output format developer, you have to remove the `encoding` argument in your `intermediates_generator` if your output format uses this function.

- The `encoding` argument is no longer passed to the `render` element of the site generator (see `?rmarkdown::render_site`).

- The `encoding` argument has been removed from many of the internal functions in the **rmarkdown** package. Now all input files are assumed to be encoded in UTF-8. If you see an error message like "Error in `FUN(arg = ...)`: unused argument (`arg = ...`)", please file an issue (with a reproducible example) to https://github.com/rstudio/rmarkdown.

- Added a new output format `context_document()` to support ConTeXT output (thanks, @RLesur #1725, @jooyoungseo #1713).

- `render_site()` can render R scripts in addition to Rmd files if you set `autospin: true` in `_site.yml` (thanks, @zeehio, #1564).

- Added `ext` argument to `md_document()`. Its default value is ".md". This argument is intended to be used together with `variant` argument (e.g., `variant = "context"` and `ext = ".pdf"`) (thanks, @atusy, #1715).

- `ioslides_presentation()` stylesheet is updated for printing. Browsers are notified that the presentation should preferentially be printed in landscape orientation and without margin (thanks, @RLesur, #1718).

- Reverted the fix #1703 and applied an alternative fix to #1700, because the original fix brought a new bug #1714 (thanks, @pablobernabeu @cderv @everron @aronatkins).

- Tabsets don't work with Pandoc 2.8 (thanks, @mnazarov, #1723).

- The `pdf_document` format failed to work if the `header-includes` field in YAML is an empty list (thanks, @cderv, #1728).


rmarkdown 1.18
================================================================================

- For `pdf_document()`, now we patch Pandoc's built-in LaTeX template to include the document subtitle (unnecessary with pandoc 2.6 onwards) and reduce the vertical spacing before title using `--include-in-header` rather than overwriting the built-in template, avoiding compability problems with newer versions of Pandoc (thanks, @adunning, #1563).

- `find_external_resources()` works now when multiple files are specified in the `includes` option of the output format (thanks, @andrie, #1677).

- `find_external_resources()` can find external resources specified in the output format's `reference_doc` or `reference_docx` option now (thanks, @jmcphers, #1696).

- `rmarkdown::run(file = NULL, dir = "foo/")` failed to run Rmd files under the `foo/` directory (thanks, @jenzopr, #1703).

- Reverted the fix for #1692 since it is no longer necessary (https://github.com/yihui/tinytex/issues/152#issuecomment-552796864).

- The `header-includes` field in the YAML metadata will no longer be overwritten by the command-line option `--include-in-header` (thanks, @crsh @mnazarov, #1359).

- Removed the `xmlns` attribute in the `<html>` tag in the default HTML template (thanks, @grady #1640, @spgarbet #995).


rmarkdown 1.17
================================================================================

- `html_vignette()` passes `self_contained` argument value to base format (thanks, @cderv, #1668).

- `find_external_resources()` works for the `html_vignette` type again, this fixes rendering vignettes with external resources in pkgdown (regression introduced in rmarkdown 1.16, #1668).

- `render(..., clean = TRUE)` may fail to clean the `*_files` directory when the output format is `prettydoc::html_pretty` (thanks, @yixuan, #1664).

- For `ioslides_presentation`, images with atributes (e.g., `![](sample.png){width=80%}`) can be correctly embedded in the self-contained mode now (thanks, @hadley, #1687).

- Fixed the Pandoc LaTeX templates to avoid the error `File `grffile.sty' not found`. This is because the LaTeX **grffile** is no longer available in TeX Live (thanks, @cderv #1691, @smmurphy #1692, @JacobD05 https://github.com/yihui/tinytex/issues/152).


rmarkdown 1.16
================================================================================

- The `pandoc-citeproc` binary can now be found correctly on Windows. This fixes an issue with `pandoc_citeproc_convert()` (thanks @cderv, #1651).

- Added `self_contained` argument to `html_vignette` to keep intermediate directory if `self_contained = FALSE` (thanks, @cderv, #1641).

- It is now possible to add pagebreak in HTML, Word, LaTeX, and ODT documents using the `\newpage` or `\pagebreak` command in an Rmd file. This is possible thanks to the [Pandoc's pagebreak Lua filter](https://github.com/pandoc/lua-filters/tree/master/pagebreak). See `vignette("lua-filters", package = "rmarkdown")` (thanks, @cderv, #1626).

- The Pandoc extension `ascii_identifiers` is no longer enabled by default. If you still need it, you may use the argument `md_extensions = "+ascii_identifiers"` in the output format function. However, please note that this will trigger an error in a future version of Pandoc.

- Output formats can be configured by arbitrary YAML files, which used to be restricted to `_output.yml` or `_output.yaml`. They can be specified via the `output_yaml` argument of `render()` or the `output_yaml` top-level parameter of YAML front matter, and the first existing one will be used. If `output_yaml` is specified both for `render()` and YAML front matter, then `render()` has the priority. If none are found, then `_output.yml` or `_output.yaml` will be used if they exist (thanks, @atusy, #1634).

- Added a Pandoc Lua filter to convert fenced Divs to LaTeX environments when the output format is `latex` or `beamer`. Basically a fenced Div `::: {.NAME data-latex="[OPTIONS]"}` is converted to `\begin{NAME}[OPTIONS] \end{NAME}` in LaTeX. The attribute `data-latex` must be provided, even if it is an empty string (meaning that the LaTeX environment does not have any optional arguments). For example, `::: {.verbatim data-latex=""}` generates a `verbatim` environment, and `::: {.minipage data-latex="{.5\textwidth}"}` generates `\begin{minipage}{.5\textwidth}`. This Lua filter was originally written by @RLesur at https://github.com/yihui/bookdown-crc/issues/1. It will allow users to create custom blocks that work for both HTML and LaTeX output (e.g., info boxes or warning boxes).

- Added `keep_html` argument to `github_document` so to save a preview HTML file in a working directory (thanks, @atusy, #1650).


rmarkdown 1.15
================================================================================

- Exclude `README.R?md` from files processed by `render_site()`,

- `html_document` with `code_folding: hide` supports showing individual source code chunks if they are assigned the `fold-show` class via the chunk option `class.source="fold-show"` (thanks, @atusy, #1602).

- The `extra_dependencies` argument only works with `template: default` in `pdf_document`. Now it works with any Pandoc LaTeX templates as long as the template uses the `header-includes` variable.


rmarkdown 1.14
================================================================================

- Fixed a regression in `ioslides_presentation` that background colors via the `data-background` attribute on slides stopped working (thanks, @ShKlinkenberg, #1265).

- Fixed the bug #1577 introduced in **rmarkdown** v1.12: tabsets, floating TOC, and code folding in the `html_document` format no longer work with the `shiny` runtime (thanks, @RLesur for the fix #1587, and @fawda123 @ColinChisholm @JasonAizkalns for the bug report).

- Added the `keep_md` argument to `pdf_document()` to keep the intermediate `.md` output file (thanks, @broomej, #1001).

- For `render()`, if the input filename contains special characters such as spaces or question marks (as defined in `rmarkdown:::.shell_chars_regex`), the file will be temporarily renamed with the special characters replaced by `-` (dash) instead of `_` (underscore, as in previous versions of **rmarkdown**). This change will affect users who render such files with caching (cache will be invalidated and regenerated). The change is due to the fact that `-` is generally a safer character than `_`, especially for LaTeX output (#1396).

- Added a **pkgdown** site for the **rmarkdown** package: https://rmarkdown.rstudio.com/docs/ (thanks, @apreshill, #1574).

- Fixed the bug #1593: in HTML documents, when a MathJax URL is used with a custom template, the source code of the MathJax library is included in the document. This bug was first declared in **bookdown** (thanks, @topepo for the bug report rstudio/bookdown#683, and @RLesur for the fix #1594).


rmarkdown 1.13
================================================================================

- For `pdf_document()`, do not override margins to 1 inch when a custom document class or geometry settings are specified in the YAML front matter (thanks, @adunning, #1550)

- The default value of the `encoding` argument in all functions in this package (such as `render()` and `render_site()`) has been changed from `getOption("encoding")` to `UTF-8`. We have been hoping to support UTF-8 only in **rmarkdown**, **knitr**, and other related packages in the future. For more info, you may read https://yihui.org/en/2018/11/biggest-regret-knitr/.

- The option `toc_float: true` for `html_document` now preserves the text formatting (thanks, @codetrainee, #1548).

- For the `output_file` argument of `render()`, a file extension will be automatically added if the filename does not contain an extension (e.g., `render('foo.Rmd', 'html_document', output_file = 'bar')` will generate `bar.html`); see the help page `?rmarkdown::render` for details (thanks, @apreshill, #1551).

- TOC items are not correctly indented when `toc_float` is enabled for the `html_document` format (thanks, @carolynwclayton #1235 and @RLesur #1243).

- Fixed rstudio/shiny#2307 where the second execution of a `shiny_prerendred` document with `href` dependencies would cause a prerender check error (thanks, @schloerke, #1562).

- The `*_files` directory is not properly cleared due to the inappropriate fix for #1503 and #1472 in the last version (thanks, @wxli0 #1553, @cderv #1566).

- Added an `output_extensions` argument to `pdf_document()` to make it possible to enable/disable Pandoc extensions for the LaTeX output format (thanks, @hongyuanjia, rstudio/bookdown#687).


rmarkdown 1.12
================================================================================

* Fixed file extensions of output files when using non-markdown Pandoc extensions such as `docx+styles` (#1494, @noamross).

* Added a new argument `extra_lines` to `latex_dependency()` to allow users to add extra lines of LaTeX code after `\usepackage{}`. Also added a helper function `latex_dependency_tikz()` based on `latex_dependency()` (#1502, @malcolmbarrett).

* Fixed #1529: when the path of an Rmd file contains Unicode characters that cannot be represented in the system native encoding (especially on Windows), `rmarkdown::render()` may fail.

* Applied a correct fix to an old **plotly** issue ropensci/plotly#463.

* HTML widgets used to be hidden when printing ioslides to PDF in Chrome. Now they will be printed correctly.

* `render(output_format = 'all')` may delete the figure directories that are still needed by certain output formats when one output format doesn't need its figure directory (thanks, @rmcd1024 #1472, @cderv #1503).

* The `<em>` tags in the subtitle, date, and author are removed from the default HTML template (thanks, @royfrancis, #1544).


rmarkdown 1.11
================================================================================

* Fixed #1483, which prevented the triangle to be displayed in Firefox when `<details><summary>...</summary></details>` was used (#1485, @bisaloo)

* Provided `rmarkdown.pandoc.args` as a **knitr** package option in `knitr::opts_knit` (#1468, @noamross).

* Added the exported function `pandoc_exec()`, which returns the path of the pandoc binary used by the package (#1465, #1466 @noamross).

* `new_session: true` in `_site.yml` causes `render_site()` to render each file in a new R session, eliminating some cross-file difficulties, such as function masking (#1326, #1443 @jennybc).

* Added the LaTeX command `\passthrough` in the default LaTeX template for the `--listings` flag of Pandoc (rstudio/bookdown#591).

* The icons in `flexdashboard::valueBox()` are not of the full sizes due to the upgrade of FontAwesome in #1340 in the previous version (#1388, rstudio/flexdashboard#189).

* Added the ability to generate tabset dropdowns, usable by adding the `.tabset-dropdown` class to a header (e.g., `# Heading {.tabset .tabset-dropdown}`) (#1405). Thanks @stefanfritsch for contributing the necessary code for this (#1116).

* The `darkly` theme (a darker variant of the Bootswatch `flatly` theme) has been added to `html_document` and `html_notebook` (#1409, #889).

* Fixed a regression that caused scrollbars on code blocks when the syntax highlighting theme is not the default (#654, #1399).

* Fixed #1407: reactive expressions can break the section headers of Shiny R Markdown documents.

* Fixed #1431: `render()` with the `intermediates_dir` argument when the output format is `powerpoint_presentation` with a custom `reference_doc` fails to find the reference document.

* Fixed the website navbar not being able to display submenus properly (#721, #1426).

* Added checks for shiny-prerendered documents to find all html dependencies, match all execution packages, and match the major R version (#1420).

* Added an argument `cache = TRUE` to the internal function `rmarkdown:::find_pandoc()`, so that users can invalidate the cached path of Pandoc via `rmarkdown:::find_pandoc(cache = FALSE)` (thanks, @hammer, #1482).

* Added an RStudio project template for simple R Markdown websites, so that users can create such websites from RStudio: `New Project -> New Directory -> Simple R Markdown Website` (thanks, @kevinushey, #1470).

* Fixed #1471: Pandoc's (version 2.x) syntax highlighting themes don't work well with the Bootstrap style (thanks, @gponce-ars #1471, @cderv #1489).

* Fixed the warnings in #1224 and #1288 when calling `render()` with an absolute `output_dir` or `intermediates_dir`.

* Fixed #1300: calling `render()` with `intermediates_dir` may fail when the intermediate dir is on a difference device or filesystem.

* Fixed #1358: calling `render()` with `intermediates_dir` will fail if the Rmd document contains bibliography files that are dynamically generated.


rmarkdown 1.10
================================================================================

* Added a new argument `slide_level` to `powerpoint_presentation()` (#1270).

* The **tinytex** package has become a required dependency (to build R Markdown to PDF).

* Added `compact-title` variable to the LaTeX default templates to control use of LaTeX `titling` package; defaults to `true` (#1284).

* `pdf_document(template = NULL)` does not work (#1295).

* Restore ability to use any HTML format with R Markdown Websites (#1328).

* Add `options` argument to `paged_table()` to enable explicit passing of display options.

* Add `pandoc_citeproc_convert()` function for conversion of bibliography files (e.g. BibTeX files).

* Update to Font Awesome version 5.0.13 (#1340).

* Add `site_resources()` function for computing resource files required for a website.

* Export `default_site_generator()` function.

* The `latex_document()` format should not clean up the figure directory (thanks, @emiltb, rstudio/bookdown#582).

* Enable post processors that change the output file to specify that the base post processor should still be applied to the original output file.


rmarkdown 1.9
================================================================================

## NEW FEATURES

* Added a new (experimental) output format `powerpoint_presentation`. If you want to test it, you will need Pandoc >= 2.1 (#1231).

## MAJOR CHANGES

* If the **tinytex** package is installed, PDF output is built through `tinytex::latexmk()`, otherwise it is generated by `rmarkdown:::latexmk()`, which has been factored out and improved in the **tinytex** package, so it is recommended that you install the **tinytex** package (#1222).

## BUG FIXES

* Temporary files created in `render()` may be cleaned up prematurely, which can cause problems with Shiny R Markdown documents (#1184).

* Further improvements regarding compatibility with Pandoc 2.0, e.g. tabbed sections don't work (https://community.rstudio.com/t/3019).

* When `preserve_yaml = TRUE` in `md_document()`, `toc = TRUE` fails to create the table of contents (thanks, @stla, #1216).

* Suppress confusing error messages from `knitr::purl()` during `rmarkdown::find_external_resources()` (thanks, @aronatkins #1247, and @paulobrecht #1154).

* Fixed the obscure error `Error: path for html_dependency not found:`, which was due to the HTML dependency of highlight.js (thanks, @bborgesr, #1213).


rmarkdown 1.8
================================================================================

## BUG FIXES

* `render_site()` does not work with `_site.yml` that does not have the `output` setting (#1189).

* The variables `input` and `output` do not work in Shiny R Markdown documents (#1193).

* `ioslides_presentation` fails to embed images (#1197).

* With Pandoc 2.x, `github_document()` generates the wrong filename extension `.gfm-ascii_identifiers` instead of `.md`, and line height of code blocks in the HTML preview is too big (#1200).


rmarkdown 1.7
================================================================================

* Fixed an issue with `df_print: paged` where row names where not printed and added support for `rownames.print` option to control when they print.

* Add `smart` option for `word_document()` format.

* Save render intermediates when generating beamer presentations (fixes #1106).

* Fixed issues when specifying NULL/null/empty parameter values (#729 and #762).

* Better error message when unable to prerender a document. (#1125)

* `shiny::renderText()` does not work in Markdown section headings (#133).

* The `value` argument of `pandoc_variable_arg()` can be missing now (#287).

* Background colors and images are supported for ioslides presentations (#687).

* HTML widgets in an Rmd document cannot be rendered if another Rmd document is rendered via `rmarkdown::render()` in this document (#993).

* Try harder to clean up temporary files created during `render()` (#820).

* Wrong environment for evaluating R code chunks in Shiny R Markdown docs (#1162, #1124).

* Do not call `bibtex` to create the bibliography when there are no citations in the document and the output format is `pdf_document()` with `citation_package = 'natbib'` (#1113).

* `render()` will stop if the output format is PDF but there are any errors during building the index or bibliography (#1166).

* `beamer_presentation()` doesn't work when `citation_package != 'none'` (#1161).

* File-based inputs don't work in parameterized documents (#919).

* rmarkdown is compatible with Pandoc 2.0 now (#1120).

* `render()` with `intermediates_dir` fails with R plots (#500).

* Added two new output formats `latex_document()` and `latex_fragment()` (#626).

* Relative paths of images in HTML output should not be resolved to absolute paths (#808).

* `render_site()` does not support multiple output formats for a single Rmd (#793).

* Unicode characters may be scrambled when downloading the Rmd source file using the download button generated by `html_document(code_download = TRUE)` (#722).

* Upgraded highlight.js from v1.1 to v9.12.0 (#988, #907).

* The argument `keep_md = TRUE` actually preserves the Markdown output file from `knitr::knit()` now (as documented). Previously, it generates a new Markdown file by concatenating the YAML metadata (title, author, date) with the body of the original Markdown output file (#450).

* For `md_document()`, when `variant == 'markdown'` and `perserve_yaml = TRUE`, the Pandoc argument `--standalone` should not be used (#656).


rmarkdown 1.6
================================================================================

* Fixed an issue where headers with non-ASCII text would not be linked to correctly in the table of contents.

* Support code folding for bash, python, sql, Rcpp, and Stan chunks.

* Provide rmarkdown.pandoc.id_prefix as Knit option

* Fixed two issue with `df_print: paged`, one would prevent rendering data with lists of lists and the other where the column type would get cut off.

* Better support for `citation_package: biblatex` in `pdf_document()` (#1062).

* On certain Windows platforms, compiling LaTeX to PDF may fail because `system2(stdout = FALSE)` is not supported, in which case the default `system2()` will be used (#1061).

* Allow paged tables to render even when page load / visibility has a long delay


rmarkdown 1.5
================================================================================

* Fixed an issue where code within Shiny pre-rendered documents was not rendered correctly.

* Add `includes` parameter to `html_fragment` format.

* Use RStudio redirection URL to replace deprecated MathJax CDN


rmarkdown 1.4
================================================================================

* `data.table` expressions involving `:=` are no longer automatically printed within R Markdown documents. (#829)

* Fix #910: the extra_dependencies argument of pdf_document() does not work when no code chunks contain LaTeX dependencies.

* The extra_dependencies of pdf_document() can also take a character vector of LaTeX package names, or a named list of LaTeX package options (with names being package names), which makes it much easier to express LaTeX dependencies via YAML.

* Automatically ignore packrat directory for render_site

* Fix #943: escape end tags in shiny_prerendered code contexts

* Add support for sticky tabs in html_document via tabset-sticky class

* Process Rmd files with lowercase extension (.rmd) in render_site

* Fix stack space consumption issues with large JS payloads in chunks

* Add `section_divs` argument to html_document and html_fragment to control generation of section tags/divs.

* Remove data-context="(data|server|server-start)" chunks from HTML served to client in shiny_prerendered


rmarkdown 1.3
================================================================================

* Fix v1.2 regression in ordering of CSS for ioslides_presentation.

* Fix rendering of pagedtables within html_notebook format.

* Ensure that html_prerendered UI is never cached.

* Add `citeproc` argument to YAML header; controls whether pandoc-citeproc is used (#831)


rmarkdown 1.2
================================================================================

* Add support for df_print to handle additional dplyr classes: grouped_df, rowwise_df and tbl_sql.

* Add new `runtime: shiny_prerendered` mode for interactive documents.

* Prepend "section-" to ids in runtime: shiny[_prerendered] to eliminate potential conflicts with shiny output ids

* Use html_dependencies for highlight.js, pagedtables, slidy, ioslides, & navigation (improved dependency behavior for runtime: shiny[_prerendered])

* Serialize runtime: shiny[_prerendered] dependencies to JSON rather than .rds

* ioslides: check for null before concatenating attr["class"] (#836)

* Add rmarkdown.onKnit/onKnitCompleted package hooks

* Non-ASCII keys in yaml file should be marked to UTF8 as well, when read into R as the name of a list (#841)

* Remove key-column special-case left alignment in pagedtables (#829)

* Replace backslashes in floating TOC headings (#849)

* Suggests rather than Imports for tibble (R 3.0 compatibility)

* Add `paged_table` function for printing paged tables within HTML documents

* Support `{.active}` attribute for setting initially active tab (#789)

* Add `knit_root_dir` argument to `render()` and YAML header, a convenience for setting knitr's root.dir option

* Improve alignment of text in sub-topics for floating TOC

* Bibliography file paths in YAML containing forward slashes could not be rendered (#875)


rmarkdown 1.1
================================================================================

* Fixed an issue where attempts to render an R Notebook could fail if the path contained multibyte characters.

* Fixed an issue where the default Beamer template did not provide vertical padding between paragraphs with certain versions of pandoc (<= 1.17.2).

* Try to install latexmk automatically on Windows

* Added `df_print` option to html_document format for optionally printing data frames using `knitr::kable`, the `tibble` package, or an arbitrary function

* Fix for render_site not showing Chinese characters correctly

* Fix for ignoring knit_meta that is explicitly passed to render

* Parameter editing: don't allow NULL to overwrite previous state

* Parameter editing: fix incorrect name for parameters with expressions

* Parameter editing: allow multiple values when the parameter is configured to use a "multiple" selector

* Switched the order in which format dependencies are added for `html_document` so that `extra_dependencies` are added at the end, after bootstrap, etc. (#737)

* `pdf_document(keep_tex = TRUE)` will generate the .tex document even if PDF conversion fails (#779).

* Move latex header includes to just before \begin{document}

* Special 'global' chunk label for runtime: shiny which designates a chunk to be run once and only once in the global environment (startup performance improvement for multi-user shiny documents)

* Ensure supporting files are writeable (#800)

* Make the "show code" buttons more CSS-friendly (#795)

* Exclude `output_dir` from site files (#803)

* Export `navbar_html` and `yaml_front_matter` functions


rmarkdown 1.0
================================================================================

* `toc_float` no longer automatically sets `toc = TRUE`

* Added an argument `error` to `pandoc_available()` to signal an error when (if `error = TRUE`) pandoc with the required version is not found.

* Added `html_notebook` format for creating HTML documents that include source code and output.

* Added `resolve_output_format` function (useful for front ends that need to mirror the default format resolution logic of `render`).

* Added `code_download` option to `html_document` to provide an option to embed a downloadable copy of the Rmd source code within the document.

* Added `slide_level` option to ioslides_presentation to set the level of heading used for individual slides.

* Added `hard_line_breaks` option to `github_document` to deal with change in behavior of GitHub's markdown renderer with respect to line breaks.

* Use "markdown_strict" rather than "markdown" for `pandoc_self_contained_html` when pandoc >= 1.17 (pandoc hanging bug was fixed in this version)

* Default highlighting engine for `html_document` now highlights bash, c++, css, ini, javascript, perl, python, r, ruby, scala, stan and xml

* Added `print` sub-option to `toc_float` to control whether the table of contents appears when user prints out the HTML page.

* Added `readme` option to `html_vignette` which automatically creates a package level README.md (for GitHub) in addition to rendering the vignette.

* Support for `keep_md` in `html_vignette` format.

* Try to install the latexmk package automatically on Windows if the executable latexmk.exe exists.


rmarkdown 0.9.6
================================================================================

* Ability to set `opts_hooks` in `knitr_options()` (#672)

* Added `render_site` and related functions for rendering collections of documents within a directory as a website.

* Ability to define html_document navigation bar using simple yaml format.

* Added `pre_knit` and `post_knit` hooks for output formats.

* Discover `LaTeX` dependencies and add them to the `.tex` preamble (#647)

* Added new `all_output_formats` function to enumerate all output formats registered for an Rmd.

* Change `fig_caption` default to TRUE for all formats

* Change `fig_retina` to 2 for HTML formats (no longer contingent on `fig_caption`)

* Ensure pandoc binary exists before binding to pandoc directory (#632)

* Handle relative paths for 'default_output_format' (#638)

* Eliminate duplicate viewport meta tag from html_document

* Added biblatex biblio-style support to the LaTeX template for Pandoc 1.15.2 (#643)

* Allow override of header font-size in html_document custom css (#652)

* Fix for horizontal scrollbars appearing w/ code folding (#654)

* Specifying `toc_float` in `html_document` now automatically sets `toc = TRUE`

* Enable per-header opt-out of `toc-float` via {.toc-ignore} attribute

* Correctly handle soft line breaks in ioslides_presentation (#661)

* Don't use text-muted for code folding btns (text visibility in non-default themes)

* Fix for rendering non-HTML formats from .md files (resolve runtime before knit)

* `html_dependency_bootstrap` now accepts theme = "default" argument

* Use pandoc 1.17.0.2 compatible LaTeX template when pandoc >= 1.17.0.2

* Support custom template for `ioslides_presentation`

* Added `analytics` option for `ioslides_presentation` for Google Analytics

* Removed the extra tag `<p></p>` around HTML output (typically generated by htmltools) from code chunks, to avoid invalid HTML like `<p><div>...</div><p>` (#685)


rmarkdown 0.9.5
================================================================================

* Added odt_document format for OpenDocument Text output

* Added rtf_document format for Rich Text Format output

* Added github_document format for GitHub Flavored Markdown output

* Only apply white background for themed HTML documents (#588)

* Added <meta name="viewport" content="width=device-width, initial-scale=1"> to the default HTML template to make it work better with mobile browsers. (#589)

* Specify --filter pandoc-citeproc after custom pandoc args

* Long lines in code blocks will be wrapped in the `html_vignette()` output (#595)

* Added new arguments `run_pandoc = TRUE` and `knit_meta = NULL` to `render()`. See the help page of `render` for details. (#594)

* The `tufte_handout` format now delegates to the `tufte` package and no longer provides a base template.

* Use pandoc 1.15.2 compatible LaTeX template when pandoc >= 1.15.2

* Fix issue with Beamer template and pandoc 1.15.2

* Updated embedded JQuery to v1.11.3 and Bootstrap to v3.3.5.

* Expose core HTML dependencies for use by custom R Markdown formats.

* New `html_document` themes: "lumen", "paper", "sandstone", "simplex", & "yeti".

* Ability to include bootstrap navbar for multi-page `html_document` websites

* Added support for `abstract` field to `html_document` format

* Added support for floating table of contents (via tocify) to `html_document`

* Added support for tabsets via use of {.tabset} class on top-level headings

* Added support for folding/unfolding of R code chunks in `html_document`

* Support `url` references in CSS files for `runtime: shiny`

* Change name of common options file to `_output.yml`

* Tweak pandoc conversion used in `pandoc_self_contained_html` to prevent hanging with large script elements (use "markdown" rather than "markdown-strict" as input format)

* The filename extension .bib will be removed before bibliography files are passed to Pandoc when the output is LaTeX/PDF and the citation package natbib or biblatex is used on Windows. This is because bibtex in MikTeX will always add the extension .bib to bibliography files, e.g. it treats foo.bib as foo.bib.bib. (#623)

* Render Shiny documents in a clean environment; fixes issue in which code in Shiny documents could access internal R Markdown state


rmarkdown 0.9.2
================================================================================

* Added a fix to #580 for Windows users.


rmarkdown 0.9.1
================================================================================

* Fix for a bug causing certain files to be deleted as intermediate files. (#580)

* PDF/LaTeX output no longer uses natbib as the citation package by default. If you do want to use natbib or biblatex, you may still use the argument citation_package = 'natbib' or 'biblatex'. (#577)


rmarkdown 0.9
================================================================================

* Fix for JS exception in slidy_presentation when served from the filesystem (don't call pushState for file:// urls)

* Escape single quotes in file paths

* Fix for missing resources when rendering a filename with shell characters

* For PDF/LaTeX output, citations are processed via natbib or biblatex instead of pandoc-citeproc. The ciation package natbib or biblatex can be chosen using the argument `citation_package` in `pdf_document()`, `beamer_presentation()`, and `tufte_handout()`. LaTeX is compiled to PDF via latexmk (https://www.ctan.org/pkg/latexmk/); if it is not installed, a simple emulation will be used (run pdflatex/xelatex/lualatex, bibtex, and makeindex a few times). You are recommended to install latexmk, and please note latexmk requires a Perl installation (this is important especially for Windows users).

* Always use the graphics package for PDF output

* Fix for the error "cannot change value of locked binding for 'metadata'" when one call of rmarkdown::render() is nested in another one (#248)

* Fix for an issue causing image paths to be rendered incorrectly in Windows when rendering an `html_document` with `self_contained: false` and a path is passed in argument `output_dir`. (#551)

* Add always_allow_html option for preventing errors when html_dependencies are rendered in non-HTML formats (e.g. pdf_document or word_document).

* Fix for an issue causing resources not to be discovered in some documents containing an empty quoted string (`""`) in an R chunk.


rmarkdown 0.8.1
================================================================================

* Support for table of contents in word_document (requires pandoc >= 1.14)

* Support for opts_template in knitr options

* Don't implicitly discover directories when scanning for dependent resources

* Fix for slide numbers not showing up in ioslides when served from the filesystem (don't call pushState for file:// urls)

* Remove inlining of bootstrap CSS (was workaround for bug now fixed in pandoc)

* Allow specifying an R file in calls to find_all_resources


rmarkdown 0.8
================================================================================

* Add support for keep_md to word_document

* Increase pandoc stack size to 512M (often required for base64 encoding e.g. larger embedded leaflet maps). Stack size can also be controlled by the pandoc.stack.size option.

* Allow YAML front matter to be terminated with ...

* Automatically generate a user-interface (via a Shiny application) for user specification of values in parameterized reports.

* Add tightlist macro for compatibility with pandoc >= 1.14

* Bugfix: Don't merge render params recursively with knit_params

* Bugfix: Handle slashes correctly on Windows for slidy_presentation when self_contained = FALSE


rmarkdown 0.7
================================================================================

* Add latex_engine option to beamer_presentation format

* Ensure that when LANG=en_US pandoc receives en_US.UTF-8 (prevent hang)

* Generate MathJax compatible output when using html_fragment format.

* Use pandoc built-in template for Beamer

* Use pandoc 1.14 compatible LaTeX template when pandoc >= 1.14

* Inline bootstrap.min.css to workaround pandoc 1.14 base64 encoding issue

* Add support for discovering references to external resources when the document output format is PDF

* Fix several issues causing pandoc errors when an intermediates directory is used, including during render for Shiny documents


rmarkdown 0.6
================================================================================

* Support for parameterized reports. Parameter names and default values are defined in YAML and can be specified via the 'params' argument to the render function

* 'md_extensions' option to enable/disable markdown extensions for input files

* Automatically discover and include dependent resources (e.g. images, css, etc.) for interactive documents.

* Added pandoc_version function.

* Use VignetteEncoding directive in html_vignette format

* Fix issues related to use of non-ASCII characters in ioslides_presentation

* Non-ASCII characters in the YAML data are not marked with the UTF-8 encoding when they are read into R, so character strings in `rmarkdown::metadata` may be displayed incorrectly (#420)

* Various improvements to tufte_handout format


rmarkdown 0.5.1
================================================================================

* Add 'dev' option to output formats to specify output device for figures

* Enable use of footnotes in titles of LaTeX documents

* Various improvements related to directory detection/handling on Windows


rmarkdown 0.4.2
================================================================================

* Sync to the latest LaTeX and Beamer templates from pandoc-templates

* Switched from the Bootstrap 2 web framework to Bootstrap 3. This is designed to work with Shiny >= 0.10.3, which has made the same switch

* Add CSS to restore responsive image behavior from Bootstrap 2

* Use a more subtle treatment for inline code in Bootstrap themed documents

* Improved support for multiple authors in ioslides

* Workaround for poor rendering of ioslides multi-columns lists in Safari 8

* Serve index.html as fallback default file for rmarkdown::run


rmarkdown 0.3.11
================================================================================

Initial release to CRAN
