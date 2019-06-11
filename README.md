# pixiplex
[![package status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

`pixiplex` is an [experimental](https://www.tidyverse.org/lifecycle/#experimental) [R package](http://r-pkgs.had.co.nz/intro.html) aimed at enabling _interactive_ and _efficient_ visualization of simplicial complexes and graphs. The package exports a smaller wrapper around a subset of the [pixi.js](https://www.pixijs.com/) libraries, which is connected to R via the [htmlwidgets](https://shiny.rstudio.com/articles/htmlwidgets.html) framework.

The visualizations are rendered _interactively_ in the sense that a variety of common viewport interactions are supported out-of-the-box, e.g. panning, zooming (including pinch-to-zoom), dragging (+deceleration), snapping, clamping, [force-driven simulations](https://en.wikipedia.org/wiki/Force-directed_graph_drawing) using [d3-force](https://github.com/d3/d3-force), support for [reactive observers](https://shiny.rstudio.com/reference/shiny/0.14/observe.html) with [shiny](https://shiny.rstudio.com) as well as variety of customizable [message handlers](https://shiny.rstudio.com/articles/js-send-message.html), etc. 

The visualizations are rendered _efficiently_ in the sense that simplex rendering is done using [WebGL](https://www.khronos.org/webgl/) primitives with help of [pixi.js](http://www.pixijs.com/), from which the package derives its name from. 

This package is in a very early stage of development. 

## Quickstart 

```R
library("pixiplex")
g <- igraph::sample_grg(100, radius = 0.20) # igraph used just to generate a graph
pp <- pixiplex(g)
plot(pp)
```

Enable force simulation

```R
plot(pp) %>% enableForce()
```

Change colors / other properties of nodes, 

```R
plot(pp) %>% setNodeStyle(node_ids = 1:50, color = rgb(1, 0, 0))
```

## TODO 

- Export polygon primitive to visualize higher order simplices
- Finish vignette describing features 
- more documentation
