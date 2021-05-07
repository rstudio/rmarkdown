---
title: "Interactive Documents: Advanced Topics"
output:
  html_document:
    toc: true
---

## Shiny Reactives

Interactive documents can also contain reactive expressions (useful when a piece of dynamic data is used in several places). As in Shiny applications, these values respond to changes in their inputs.

<pre class="markdown"><code>&#96;&#96;&#96;{r, echo = FALSE}
selectInput("dataset", "Choose Dataset:", c("cars", "iris", "mtcars"))

activeDataset <- reactive({
  get(input$dataset, pos="package:datasets", inherits=FALSE)
})

renderTable({
  head(activeDataset(), 5)
})

renderPlot({
  plot(activeDataset())
})
&#96;&#96;&#96;
</code></pre>

Note that reactive expressions can be used anywhere, including in the definition of inline Shiny applications using the `shinyApp` function. To learn more about reactive expressions, see the [Shiny Tutorial](http://shiny.rstudio.com/articles/basics.html).

## Multiple Pages
 
You can link to other interactive documents by using the markdown link syntax and specifying the *relative* path to the document, e.g. `[Another Shiny Document](another.Rmd)`.
 
Currently, only one document can be active at a time, so documents can't easily share state (although some primitive global sharing is possible via `global.R`; see the help for `rmarkdown::run`). 
 
By default it's only possible to link to R Markdown files in the same directory subtree as the file on which `rmarkdown::run` was invoked (i.e you can't link to `../foo.rmd`.) You can use the `dir` argument to `rmarkdown::run` to indicate the directory to treat as the root. 


## Delayed Rendering

An interactive document is generally rendered every time it is shown, and is not shown to the user until render is complete. Consequently, a document that is large or contains expensive computations may take some time to load. 

If your document contains interactive Shiny components that don't need to be rendered right away, you can wrap Shiny code in the `rmarkdown::render_delayed` function. This function saves its argument until the document is done rendering and has been shown to the user, then evaluates it and injects it into the document when the computation is finished.

Here's an example that demonstrates how `render_delayed` works---the code enclosed within the `render_delayed` call will execute only after the document has been loaded and displayed to the user:

<pre class="markdown"><code>&#96;&#96;&#96;{r, echo = FALSE}
rmarkdown::render_delayed({
  numericInput("rows", "How many cars?", 5)

  renderTable({
    head(cars, input$rows)
  })
})
&#96;&#96;&#96;
</code></pre>

## Migration

The getting started example demonstrated creating a brand new interactive document. However, any R Markdown output format that produces HTML can be converted into an interacitve document. To convert an existing document:
 
- Add `runtime: shiny` to its YAML front matter. 

- Render it with `rmarkdown::run` instead of `rmarkdown::render`. 

For example, here's the front matter for a Shiny [html_document](https://bookdown.org/yihui/rmarkdown/html-document.html):

```yaml
---
title: "My Document"
output: html_document
runtime: shiny
---
```

And here's the front matter for a Shiny [ioslides_presentation](https://bookdown.org/yihui/rmarkdown/ioslides-presentation.html):

```yaml
---
title: "My Document"
output: ioslides_presentation
runtime: shiny
---
```

As described above, once you've added `runtime: shiny` to the document you can run it using either the **Run Document** command in RStudio or using the `rmarkdown::run` function. By default, documents are re-rendered on save, so once you've got a browser open with the document loaded, just save the R Markdown file to see your changes.
