## api.R 
## Contains the API for the pixiplex R package.
## Author: Matt Piekenbrock

# Unexported helper functions
normalize <- function(x) { (x - min(x))/(max(x) - min(x)) }

## Generic meta-wrapper for passing commands from R to JavaScript
## 1. The reactive domain is null, in which case shiny is not being used. Call the widget method.
## 2. The reactive domain is not null, shiny is being used, use the appropriate message handler
## Props to https://deanattali.com/blog/htmlwidgets-tips/#api-abstract for coming up with a nice meta-wrapper
## for handling both shiny-contexts and magrittr-piped contexts
callJS <- function() {
  message <- as.list(parent.frame(1)[["export"]])
  if (methods::is(message$id, "pixiplex_vis")) {
    widget <- message$id
    message$id <- NULL
    widget$x$api <- c(widget$x$api, list(message))
    return(widget)
  } else if (is.character(message$id)){ ## Shiny mode
    session <- shiny::getDefaultReactiveDomain()
    method <- sprintf("%s:%s", message$id, message$method) # paste0("pixiplex_vis:", message$method)
    cat(sprintf("Sending message: %s \n", method))
    session$sendCustomMessage(method, message)
    return(message$id)
  } else if (methods::is(message$id, "pixiplex_active")){
    widget <- message$plot()
    message$id <- NULL
    widget$x$api <- c(widget$x$api, list(message))
    return(widget)
  } else {
    if (methods::is(message$id, "pixiplex")){
      stop("A pixiplex object was passed, however a pixiplex_vis object is expected. Did you mean to use plot(...) first?")
    }
    stop("Invalid JS method called.")
  }
}

#' Insert nodes. 
#' @param id Either the pixiplex's htmlwidget instance or, if in a shiny context, the container id the widget is housed in. 
#' @param nodes a data.frame with minimally an id column. Each row is a node to be inserted.
#' @description 
#' Inserts nodes into the pixiplex widget. If the nodes supplied already exist in the network, the nodes/properties are replaced.
#' To modify an existing nodes properties by id, see \code{setNodes}.
#' @seealso getDefaultJsonConfig setNodes
#' @export
insertNodes <- function(id, nodes){
  stopifnot(is.data.frame(nodes), "id" %in% colnames(nodes))
  export <- list(id = id, method = "insertNodes", nodes=nodes)
  callJS()
}

#' Remove nodes by id. 
#' @param id Either the pixiplex's htmlwidget instance or, if in a shiny context, the container id the widget is housed in. 
#' @param node_ids A vector of node ids to remove. 
#' @export
removeNodes = function(id, node_ids){
  if (is.data.frame(node_ids) && "in" %in% colnames(node_ids)){ 
    node_ids <- as.vector(node_ids$id)
  }
  stopifnot(is.vector(node_ids))
  export <- list(id = id, method = "removeNodes", node_ids = node_ids)
  callJS()
}

#' Sets node styling. 
#' @param id Either the pixiplex's htmlwidget instance or, if in a shiny context, the container id the widget is housed in. 
#' @param node_ids A vector of node ids.
#' @param color (string) node fill color.
#' @param radius (numeric) node radius. 
#' @param linesize (numeric) node outline size.
#' @param linecolor (string) color of outline. 
#' @description For the nodes given by \code{node_ids}, sets the drawing style in the corresponding Graphics
#' objects (node) each id corresponds to.  
#' @details The \code{color} and \code{linecolor} color codes are assumed to be either 7 or 9-length hexadecimal strings. 
#' Unless explicitly supplied, \code{alpha} is set if \code{color} is 9-length, opaque otherwise. A 0 \code{linesize} results 
#' in nodes not having a border. Missing or NULL style parameters keep the existing property of each node intact.  
#' @export
setNodeStyle <- function(id, node_ids, color=NULL, radius=NULL, alpha=NULL, linesize=NULL, linecolor=NULL){
  if (!missing(color)) { stopifnot(all(nchar(color) %in% c(7L, 9L))) }
  if (missing(alpha) && !missing(color) && nchar(color) == 9L){
    alpha <- as.integer(as.hexmode(substr(color, start=8L, stop=9L)))/255L
    color <- substr(color, start=1L, stop=7L)
  }
  if (!missing(color)){ color <- as.integer(as.hexmode(substr(color, start = 2L, stop = 7L))) }
  if (!missing(linecolor)){ linecolor <- as.integer(as.hexmode(substr(linecolor, start = 2L, stop = 7L))) }
  node_ids <- as.vector(node_ids)
  style <- list(color=color, radius=radius, alpha=alpha, linesize=linesize, linecolor=linecolor)
  style <- style[!sapply(style, is.null)]
  style <- as.data.frame(lapply(style, rep, length.out=length(node_ids))) ## recycle as necessary, use data.frame for row-conversion
  export <- list(id = id, method = "setNodeStyle", node_ids=node_ids, style=style)
  callJS()
}

#' @name insertLinks 
#' @title Inserts links into the widget. 
#' @description Inserts links connecting nodes with ids given by \code{source} and \code{target} columns.
#' @param id Either the pixiplex's htmlwidget instance or, if in a shiny context, the container id the widget is housed in. 
#' @param links a data.frame with 'source' and 'target' columns.
#' @export
insertLinks = function(id, links){
  stopifnot(is.data.frame(links), all(c("source", "target") %in% colnames(links)))
  export <- list(id = id, method = "insertLinks", links=links)
  callJS()
}

#' Remove selected links. 
#' @param id Either the pixiplex's htmlwidget instance or, if in a shiny context, the container id the widget is housed in. 
#' @param links a data.frame with 'source' and 'target' columns.
#' @export
removeLinks = function(id, links){
  stopifnot(is.data.frame(links), all(c("source", "target") %in% colnames(links)))
  # links <- cbind(pmin.int(links[, 1], links[, 2]), pmax.int(links[, 1], links[, 2]))
  export <- list(id = id, method = "removeLinks", links = links)
  callJS()
}

#' Sets the link colors. 
#' @param id Either the pixiplex's htmlwidget instance or, if in a shiny context, the container id the widget is housed in. 
#' @param color A vector of hexadecimal color codes. Transparency values are *not* supported. 
#' @export
setLinkStyle = function(id, links, color=NULL, lineWidth=NULL, alpha=NULL){
  stopifnot(is.data.frame(links) && all(c("source", "target") %in% colnames(links)))
  if (!missing(color)) { stopifnot(all(nchar(color) %in% c(7L, 9L))) }
  if (!missing(alpha) && nchar(color) == 9L){
    color <- substr(color, start=1L, stop=7L)
    alpha <- as.integer(as.hexmode(substr(color, start=8L, stop=9L)))/255L
  }
  if (!missing(color)){ color <- as.integer(as.hexmode(substr(color, start = 2L, stop = 7L))) }
  style <- list(color=color, lineWidth=lineWidth, alpha=alpha)
  style <- style[!sapply(style, is.null)]
  style <- as.data.frame(lapply(style, rep, length.out=nrow(links))) ## recycle as necessary, use data.frame for row-conversion
  export <- list(id = id, method = "setLinkStyle", links=links, style=style)
  callJS()
}

#' Add lass interaction to the widget. 
#' @param id Either the pixiplex's htmlwidget instance or, if in a shiny context, the container id the widget is housed in. 
#' @description adds lasso.  
#' @export
enableLasso = function(id){
  export <- list(id = id, method = "enableLasso")
  callJS()
}

#' Disables lass interaction to the widget. 
#' @param id Either the pixiplex's htmlwidget instance or, if in a shiny context, the container id the widget is housed in. 
#' @description disables lasso.  
#' @export
toggleLasso = function(id){
  export <- list(id = id, method = "toggleLasso")
  callJS()
}

#' Logs the network to the rendering console. Useful for debugging. Unexported.
logNetwork = function(id){
  export <- list(id = id, method = "logNetwork")
  callJS()
}

#' Group singletons.
#' @param id Either the pixiplex's htmlwidget instance or, if in a shiny context, the container id the widget is housed in. 
#' @param centroid an optional 2-vector of XY coordinates to group the singletons around.
groupSingletons = function(id, centroid = c(0.9, 0.1), strength = 0.18){
  export <- list(id = id, method = "groupSingletons", centroid = unname(centroid), strength = strength)
  callJS()
}

#' Splits nodes by a given labeling
#' @param id Either the pixiplex's htmlwidget instance or, if in a shiny context, the container id the widget is housed in. 
#' @param node_labels An integer vector of class labels to group nodes with. The 0 class is never positioned. See details.
#' @param label_xy A data.frame with containing for each group a label (column 1), a relative 'x' coordinate, and a relative 'y' coordinate.
#' @details TODO
splitByLabel <- function(id, node_labels, label_xy, isolate_links = TRUE){
  if (is.null(dim(label_xy)) || ncol(label_xy) != 3){ stop("'splitByLabel' expects to have a label defined in the 'label' attribute of the nodes.") }
  if (!is.data.frame(label_xy) || !all(names(label_xy) %in% c("label", "x", "y"))){ stop("'splitByLabel' expects label_xy to be a data.frame with valid 'label', 'x', and 'y' columns.")}
  label_xy <- jsonlite::toJSON(structure(apply(label_xy[, c("x", "y")], 1, as.list), names = as.character(label_xy$label)), auto_unbox = TRUE)
  if (isolate_links){
    
  }
  export <- list(id = id, method = "splitByLabel", node_labels=node_labels, label_xy=label_xy)
  callJS() 
}

#' Center the network
#' @param id Either the pixiplex's htmlwidget instance or, if in a shiny context, the container id the widget is housed in. 
#' @description Centers the pixiplex network at the center of the canvas' viewport. 
#' @export
center <- function(id){
  export <- list(id = id, method = "center")
  callJS()
}

#' Enables d3-forces. 
#' @param id Either the pixiplex's htmlwidget instance or, if in a shiny context, the container id the widget is housed in. 
#' @description Enables D3 forces to the network.  
#' @export
enableForce <- function(id){
  export <- list(id = id, method = "enableForce")
  callJS()
}

#' Disables d3-forces. 
#' @param id Either the pixiplex's htmlwidget instance or, if in a shiny context, the container id the widget is housed in. 
#' @description Disables forces in the network network.  
#' @export
disableForce <- function(id){
  export <- list(id = id, method = "disableForce")
  callJS()
}

#' Sets D3 force properties
#' @param id Either the pixiplex's htmlwidget instance or, if in a shiny context, the container id the widget is housed in. 
#' @param name The force name. 
#' @param enabled boolean, whether to enable the force given by \code{name} or not.
#' @param type a \href{https://github.com/d3/d3-force#forces}{d3 force}. 
#' @param ... force parameters. See details. 
#' @description This function allows creating (/ removing), enabling (/ disabling), and modifying d3 forces
#' acting on a pixiplex object. A force simulation must be generated first (via \code{enableForce}) for these to apply.  
#' @details This function allows near-complete control over the force in an existing d3-force simulation that is 
#' consistent the current d3-force API (v4+). Each force, once converted to JSON, is expected to follow the format:
#' \preformatted{\{ < force name > : \{ }
#' \preformatted{    enabled: < boolean >, }
#' \preformatted{    type: < force type >, }
#' \preformatted{    params: \{ < force parameters > \}} 
#' \preformatted{\}\}}
#' For example, to create the set of following forces. 
#' \preformatted{params = \{ }
#' \preformatted{    charge: \{ enabled: true, type: "forceManyBody", params: \{ strength: 30, distanceMin: 1 \} \}, }
#' \preformatted{    link: \{ enabled: true, type: "forceLink", params: \{ distance: 30, iterations: 1 \} \}, }
#' \preformatted{    center: \{ enabled: true, type: "forceCenter", params: \{ x: 0, y: 0 \} \} }
#' \preformatted{\}}
#' The first example given below shows to generate these forces. 
#' Note that multiple forces of the same \code{type}, but not the same name, are permitted. The parameters are checked to ensure they
#' are . The parameters \code{...} are converted to JSON via 
#' \code{\link[jsonlite]{toJSON}} with the arguments \code{dataframe="rows"} and \code{auto_unbox=TRUE}.
#' @seealso \code{\link{enableForce}} \code{\link[jsonlite]{toJSON}}
#' @examples
#' g <- igraph::sample_grg(100, radius = 0.20)
#' 
#' ## Simulate gravity
#' plot(pixiplex(g)) %>% enableForce() %>% 
#'   setForce("charge", enabled = TRUE, type = "forceManyBody", strength = 30) 
#' \dontrun{
#'     
#'     setForce(name="charge", type="forceManyBody", list(strength=30, distanceMin=1))
#'     setForce(name="link", type="forceLink", list(strength=30, iterations=1))
#'     setForce(name="charge", type="forceCenter", list(x=0, y=0))
#' }
#' @export
setForce <- function(id, name, enabled=TRUE, type=NULL, ...){
  stopifnot(type %in% c("forceManyBody", "forceLink", "forceCenter", "forceCollide", "forceX", "forceY", "forceRadial"))
  force_params <- list(...)
  for (param_name in names(force_params)){
    switch(type, 
      "forceManyBody" = stopifnot(param_name %in% c("strength", "theta", "distanceMin", "distanceMax")),
      "forceX"= stopifnot(param_name %in% c("strength", "x")),
      "forceY"= stopifnot(param_name %in% c("strength", "y")),
      "forceRadial"= stopifnot(param_name %in% c("strength", "x", "y", "radius")),
      "forceLink"= stopifnot(param_name %in% c("distance", "strength", "iterations")), 
      "forceCollide"= stopifnot(param_name %in% c("radius", "strength", "iterations")), 
      "forceCenter"= stopifnot(param_name %in% c("x", "y"))
    )
  }
  force <- structure(list(list(enabled=enabled, type=type, params = force_params)), names=name)
  export <- list(id = id, method = "setForce", force = force)
  callJS()
}

