# pixiplex

`pixiplex` is an experimental R package aimed at enabling _interactive_ and _efficient_ visualization of simplicial complexes and graphs. The package exports a smaller wrapper around a subset of the [pixi.js](https://www.pixijs.com/) libraries, which is connected to R via the [htmlwidgets](https://shiny.rstudio.com/articles/htmlwidgets.html) framework.

The visualizations are rendered _interactively_ in the sense that a variety of common viewport interactions are supported out-of-the-box, e.g. panning, zooming (including pinch-to-zoom), dragging (+deceleration), snapping, clamping, [force-driven simulations](https://en.wikipedia.org/wiki/Force-directed_graph_drawing) using [d3-force](https://github.com/d3/d3-force), support for [reactive observers](https://shiny.rstudio.com/reference/shiny/0.14/observe.html) with [shiny](https://shiny.rstudio.com) as well as variety of customizable [message handlers](https://shiny.rstudio.com/articles/js-send-message.html), etc. 

The visualizations are rendered _efficiently_ in the sense that simplex rendering is done using [WebGL](https://www.khronos.org/webgl/) primitives with help of [pixi.js](http://www.pixijs.com/), from which the package derives its name from. 

This package is in a very early stage of development. 

## TODO 
