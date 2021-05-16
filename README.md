
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rchitecture

<!-- badges: start -->
<!-- badges: end -->

This package is my report for the architectural recovery assignment on
the Software Architecture course at ITU in Spring 2021. The purpose is
to recover meaningful views from a software project. I went with the
default example, the
[Zeeguu-Api](https://github.com/zeeguu-ecosystem/Zeeguu-API) for
analysis.

## Approach and dependencies

After some initial struggles with parsing the python AST trees in R I
settled on the following :

-   [kavon/yaml-ast](https://github.com/kavon/yaml-ast) and
    [maligree/python-ast-explorer](https://github.com/maligree/python-ast-explorer)
    for parsing python AST into data structures.
-   📦`reticulate` for calling those python scripts from R
-   📦`data.tree` for parsing yaml (and directories) into data.tree
    objects that could then be converted into igraph objects
-   📦`igraph` behind the scenes and for implementations of different
    graph algorithms
-   📦`tidygraph` for a tidy interface to manipulate the graphs
-   📦`ggraph` for plotting graphs

Furthermore, I am using 📦`tidyverse` for most of the data manipulation
and additional plotting. The documentation website is built using
📦`pkgdown`.

## Reproduction

You can install this package from github. R dependencies should be
installed automatically for reproducing what worked out. For
dependencies for things that didn’t work out and python, some debugging
might be required. Please ask :)

``` r
remotes::install_github("benjaminschwetz/Rchitecture")
```

## Example: Zeeguu Api

For the assignment, I made some summary and a dependency view. They are
in `vignette(zeeguu_api, package = "Rchitecture)`

## What didn’t work

-   ❌ Using the structure of the ast tree. Not sure if it is my lack of
    python knowledge or lack of tree wrangling skills but I didn’t find
    a good way to use the tree for anything, except splitting the script
    code into tokens. I killed a lot of time with this.
-   ❌ Simplifying the graph using edgebundling from
    `shochastics/edgebundle`. I think my graph was too messy. for the
    algorithm. see `edgebundling_failed()`. Depends on datashader
    (python) package.

## What I didn’t have time for in the end

-   ⏱️ Making a graph of external dependencies. I started parsing out
    the requirements.txt but didn’t have time to generate a separate
    graph viz for it. See `generate_dependency_graph()`
-   ⏱ Refactor code. A lot of stuff could be reused,/ is copy pasted
    across funtions right now.
-   ⏱ Parameterize the notebook: give it a github path and autogenerate
    the viz.
