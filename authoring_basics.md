---
title: "Markdown Basics"
output:
  html_document:
    toc_float: false
---

This document provides quick references to the most commonly used R Markdown syntax. See the following articles for more in-depth treatment of all the capabilities of R Markdown:

* [R Code Chunks](https://bookdown.org/yihui/rmarkdown/r-code.html)

* [Pandoc Markdown](authoring_pandoc_markdown.html)

#### Emphasis

```markdown
*italic*   **bold**

_italic_   __bold__
```

#### Headers

```markdown
# Header 1

## Header 2

### Header 3
```

#### Lists

Unordered List:

```markdown
* Item 1
* Item 2
    + Item 2a
    + Item 2b
```

Ordered List:

```markdown
1. Item 1
2. Item 2
3. Item 3
    + Item 3a
    + Item 3b
```

#### R Code Chunks

R code will be evaluated and printed

<pre class="markdown"><code>&#96;&#96;&#96;{r}
summary(cars$dist)
summary(cars$speed)
&#96;&#96;&#96;
</code></pre>

#### Inline R Code

```markdown
There were `r nrow(cars)` cars studied
```

#### Links

Use a plain http address or add a link to a phrase:

```markdown
http://example.com

[linked phrase](http://example.com)
```

#### Images

Images on the web or local files in the same directory:

```markdown
![](http://example.com/logo.png)

![optional caption text](figures/img.png)
```

#### Blockquotes

```markdown
A friend once said:

> It's always better to give
> than to receive.
```

#### Plain Code Blocks

Plain code blocks are displayed in a fixed-width font but not evaulated

<pre class="markdown"><code>&#96;&#96;&#96;
This text is displayed verbatim / preformatted
&#96;&#96;&#96;
</code></pre>

#### Inline Code

```markdown
We defined the `add` function to
compute the sum of two numbers.
LaTeX Equations
```

#### LaTeX Equations

Inline equation:
```markdown
$equation$
```

Display equation:
```markdown
$$ equation $$
```

#### Horizontal Rule / Page Break

Three or more asterisks or dashes:

```markdown
******

------
```

#### Tables

```markdown
First Header  | Second Header
------------- | -------------
Content Cell  | Content Cell
Content Cell  | Content Cell
Reference Style Links and Images
```

#### Links

```markdown
A [linked phrase][id].
```

At the bottom of the document:

```markdown
[id]: http://example.com/ "Title"
```

#### Images

```markdown
![alt text][id]
```

At the bottom of the document:

```markdown
[id]: figures/img.png "Title"
```

#### Manual Line Breaks

End a line with two or more spaces:

```markdown
Roses are red,  
Violets are blue.
```


#### Miscellaneous

```markdown
superscript^2^

~~strikethrough~~
```
