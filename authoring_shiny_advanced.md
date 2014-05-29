---
title: "Shiny Docs: Advanced Topics"
---

## Shiny Reactives

Shiny Docs can also contain reactive expressions (useful when a piece of dynamic data is used in several places). As in Shiny applications, these values respond to changes in their inputs.

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
 
You can link to other Shiny Doc by using the markdown link syntax and specifying the *relative* path to the document, e.g. `[Another Shiny Document](another.Rmd)`.
 
Currently, only one document can be active at a time, so documents can't easily share state (although some primitive global sharing is possible via `global.R`; see the help for `rmarkdown::run`). 
 
By default it's only possible to link to R Markdown files in the same directory subtree as the file on which `rmarkdown::run` was invoked (i.e you can't link to `../foo.rmd`.) You can use the `dir` argument to `rmarkdown::run` to indicate the directory to treat as the root. 

## Shiny Widgets

It's also possible to create re-usable Shiny widgets that enable authors to embed a Shiny application within a page with a single function call. For example, the following code could be used to embed a K Means clustering application:

```r
kmeans_cluster(iris)
```

This is what the widget would look like inside a running document:

![Shiny Widget KMeans](images/shiny-widget-kmeans.png)


See the article on [Shiny Widgets](authoring_shiny_widgets.html) for additional details.

## Converting Existing Documents 

The getting started example demonstrated creating a brand new Shiny Doc. However, any R Markdown output format that produces HTML can be converted into a Shiny Doc. To convert and existing document:
 
- Add `runtime: shiny` to its YAML front matter. 

- Render it with `rmarkdown::run` instead of `rmarkdown::render`. 

For example, here's the front matter for a Shiny [html_document](html_document_format.html):

```yaml
---
title: "My Document"
output: html_document
runtime: shiny
---
```

And here's the front matter for a Shiny [ioslides_presentation](ioslides_presentation_format.html):

```yaml
---
title: "My Document"
output: ioslides_presentation
runtime: shiny
---
```

As described above, once you've added `runtime: shiny` to the document you can run it using either the **Run Document** command in RStudio or using the `rmarkdown::run` function. By default, documents are re-rendered on save, so once you've got a browser open with the document loaded, just save the R Markdown file to see your changes.
