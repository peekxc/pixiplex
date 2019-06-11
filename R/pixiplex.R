#' pixiplex
#' @param x any pixiplex-coercible object. See details.
#' @description Creates a \code{pixiplex} object. Use \code{plot.pixiplex} to plot it. 
#' @details \code{x} may be any type of graph-like object. The overloads available include: 
#' \itemize{
#'  \item{\emph{matrix}}{ must be either an (n x n) (adjacency) matrix or an (n x 2) (edgelist) matrix.}
#'  \item{\emph{igraph}}{ must be valid igraph object.}
#'  \item{\emph{SimplexTree}}{ must be valid SimplexTree object.}
#'  \item{\emph{list}}{ same format as a pixiplex object. See below.}
#' }
#' A \code{pixiplex} object is a list object that contains a data.frame with an \code{id} column accessible with the \code{nodes} name 
#' and a data.frame with \code{source} and \code{target} columns accessible with the \code{links} name.
#' 
#' This format converts to the following JSON representation when sent to the htmlwidget: 
#' (network) : { \cr
#'   nodes: [{ id: 0 }, { id: 1 }, ... ], \cr
#'   links: [{ source: 0, target: 1 }, ... ] \cr
#' } \cr
#' 
#' Theoretically, any type may be used (e.g. string) to assign node ids, so long as the same type is used in the links. 
#' @import htmlwidgets magrittr
#' @export
pixiplex <- function(x){
  if (is.matrix(x)){ as.pixiplex.matrix(x) } 
  else if (is(x, "igraph")){ as.pixiplex.igraph(x) } 
  else if (is(x, "list")){ as.pixiplex.list(x) } 
  else if (is(x, "Rcpp_SimplexTree")){ as.pixiplex.simplextree(x) }
  else { stop("Invalid object passed. Must be one of the overloaded graph-like objects.") }
}

# S3 class check
is.pixiplex <- function(x) { inherits(x, "pixiplex") }
  
#' Pixiplex
#' @description Creates a interactive visualization of a simplicial complex with pixi.js. 
#'
#' Every node must have an id, and links must have source + target specifications.
#' @param pp a pixiplex object.
#' @param width width of widget. If NULL expands to viewer width. 
#' @param height height of widget. If NULL expands to viewer height.
#' @param canvas whether to use a canvas render or a WebGL renderer. Defaults to WebGL.  
#' @param ... options to pass to \code{\link[htmlwidgets]{sizingPolicy}}.
#' @details By default, launches the widget into the default browser. 
#' @import htmlwidgets
#' @export
plot.pixiplex <- function(pp, id="pixiplex_vis", width = NULL, height = NULL, canvas=FALSE, ...) {
  stopifnot(is(pp, "pixiplex"))
  pp_src <- list(nodes=pp$nodes[,c("id", "x", "y")], links = pp$links[,c("source", "target")])
  x <- list(plex = pp_src, canvas=canvas, api = list())
  # attr(x, 'TOJSON_FUNC') <- function(x){ jsonlite::toJSON(x, dataframe="rows") }
  size_policy <- list(padding=0, viewer.padding=0, 
                      browser.fill=TRUE, viewer.fill=TRUE)
  size_policy <- modifyList(size_policy, list(...))
  widget <- htmlwidgets::createWidget(
    elementId = id, 
    name = "pixiplex_vis", x = x, 
    width = width, height = height, 
    package = "pixiplex",
    sizingPolicy = do.call(htmlwidgets::sizingPolicy, size_policy)
  )
  apply_properties(widget, pp)
}

#' Shiny bindings for pixiplex
#'
#' Output and render functions for using pixiplex within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a pixiplex
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name pixiplex-shiny
#'
#' @export
pixiplexOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, name = 'pixiplex_vis', width, height, package = 'pixiplex')
}

#' @rdname pixiplex-shiny
#' @export
renderPixiplex <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, pixiplexOutput, env, quoted = TRUE)
}

#' validatePP
#' @param pp a \code{pixiplex} object.
#' @description Checks \code{pp} to ensure it is a valid \code{pixiplex} object.
#' @details If node coordinates are not specified, they are created with \code{igraph::layout.auto}.
#' @return a valid \code{pixiplex} object. 
#' @export
validatePP <- function(pp){
  stopifnot(!is.null(pp[["nodes"]]), is.data.frame(pp[["nodes"]]))
  stopifnot(!is.null(pp[["links"]]), is.data.frame(pp[["links"]]))
  if (nrow(pp[["nodes"]] > 0)){  
    stopifnot("id" %in% colnames(pp$nodes)) 
    stopifnot(all(is.atomic(pp$nodes$id)))
  }
  if (nrow(pp[["links"]]) > 0){
    stopifnot(all(c("source", "target") %in% colnames(pp$links)))
    stopifnot(all(pp$links$source %in% pp$nodes$id), all(pp$links$target %in% pp$nodes$id))
  }
  if (is.null(pp$nodes$x) || is.null(pp$nodes$x)){ 
    n <- nrow(pp$nodes)
    ids <- pp$nodes$id
    if (!is.null(pp[["links"]])){
      edges <- as.vector(rbind(match(pp$links$source, ids), match(pp$links$target, ids)))
      ig <- igraph::graph(edges, n = nrow(pp$nodes))
    } else {
      ig <- igraph::make_empty_graph(n, directed = FALSE)
    }
    xy <- igraph::layout.auto(ig)
    pp$nodes$x <- normalize(xy[,1])
    pp$nodes$y <- normalize(xy[,2])
  }
  attr(pp, "class") <- "pixiplex"
  return(pp)
}

as.pixiplex.igraph <- function(x, ...){
  nodes <- data.frame(id=as.vector(igraph::V(x)))
  links <- structure(as.data.frame(igraph::as_edgelist(x)), names=c("source", "target"))
  validatePP(list(nodes=nodes, links=links))
}
as.pixiplex.matrix <- function(x, ...){
  if (isSymmetric.matrix(x)){
    as.pixiplex.igraph(igraph::graph_from_adjacency_matrix(x, mode = "undirected", add.colnames = NA))
  } else if (dim(x)[[2]] == 2){
    as.pixiplex.igraph(igraph::graph_from_edgelist(x, directed = FALSE))
  }
}
as.pixiplex.list <- function(x, ...){
  validatePP(x)
}

as.pixiplex.simplextree <- function(x, ...){
  nodes <- data.frame(id=x$vertices)
  if (nrow(x$edges) > 0){
    links <- structure(as.data.frame(x$edges), names=c("source", "target"))
    validatePP(list(nodes=nodes, links=links))
  } else {
    validatePP(list(nodes=nodes, links=data.frame(source=integer(0L), target=integer(0L)))) 
  }
}

#' @export
print.pixiplex <- function(x, ...){
  writeLines(c(
    "pixiplex object with", 
    sprintf("%d nodes, %d links", nrow(x$nodes), nrow(x$links))
  ))
}

## unexported helper function to compose functions
compose <- function (...) {
  fs <- lapply(list(...), match.fun)
  n <- length(fs)
  last <- fs[[n]]
  rest <- fs[-n]
  function(...) {
    out <- last(...)
    for (f in rev(rest)) {
      out <- f(out)
    }
    out
  }
}

#' apply_properties 
#' @description Apply all special properties. 
#' @param widget Either a 'pixiplex_vis' widget a shiny id. See details. 
#' @param pp The pixiplex object used to
#' @details Given a 'pixiplex_vis' widget and a 'pixiplex' object, this function applies
#' all the special visual properties a pixiplex object handles, including things like node 
#' styling, textures, etc. 
#' @export
apply_properties <- function(widget, pp){
  make_widget <- list(identity)
  
  ## Collect node properties
  node_attr <- c("radius", "color", "alpha", "linesize", "linecolor")
  if (any(colnames(pp$nodes) %in% node_attr)){
    set_ns <- function(pp){
      ns_cols <- intersect(colnames(pp$nodes), node_attr)
      ns_props <- as.list(pp$nodes[,ns_cols,drop=FALSE])
      return(function(x){
        do.call(setNodeStyle, c(list(id=x, node_ids=pp$nodes$id), ns_props))
      })
    }
    make_widget <- c(make_widget, set_ns(pp))
  }
  
  ## Collect link properties
  link_attr <- c("color", "lineWidth", "alpha")
  if (any(colnames(pp$links) %in% link_attr)){
    set_ls <- function(pp){
      ls_cols <- intersect(colnames(pp$links), link_attr)
      ls_props <- as.list(pp$links[,ls_cols,drop=FALSE])
      return(function(x){
        do.call(setLinkStyle, c(list(id=x, links=pp$links[,c("source", "target"), drop=FALSE]), ns_props))
      })
    }
    make_widget <- c(make_widget, set_ls(pp))
  }
  
  purrr::invoke(purrr::compose, make_widget)(widget)
}

#' Make active bindings
#' Creates active bindings for pixiplex objects
#' @param pp a valid pixiplex object.
#' @param id widget id. 
#' @export 
make_active <- function(pp, id = "pixiplex_vis"){
  ## Given two data.frames 'x' and 'y' checks which of the vector of given column names 
  ## are different between the two. 
  get_diff <- function(x, y, cnames = colnames(x)){
    dif <- sapply(cnames, function(cname){
      if (!is.null(y[[cname]])){ return(!identical(x[[cname]], y[[cname]])) } 
      return(FALSE)
    })
    return(names(dif)[which(dif)])
  }
  pp_active <- R6::R6Class("pixiplex_active",
    private = list( 
      .vis = NULL, .shiny_mode = FALSE,
      .vis_id = id,
      .nodes = NULL, .links = NULL, .polygons = NULL, 
      .node_attr = c("radius", "color", "alpha", "linesize", "linecolor"),
      .link_attr = c("color", "lineWidth", "alpha")
    ),
    public = list(
      initialize = function(pp){
        stopifnot("pixiplex" %in% class(pp))
        tmp <- validatePP(pp)
        # private$.vis_id <- sprintf("widget_%s", paste(format(as.hexmode(sample(256, 10, replace = TRUE) - 1), width = 2), collapse = ""))
        private$.nodes <- tmp$nodes
        private$.links <- tmp$links
        private$.vis <- plot(tmp, id=id)
        private$.shiny_mode <- !is.null(shiny::getDefaultReactiveDomain())
      }, 
      plot = function(){ return(private$.vis) }
    ),
    active = list(
      nodes = function(value){
        if (missing(value)){ return(private$.nodes) }
        else {
          stopifnot(is.data.frame(value))
          ## Node insertions / removals
          n <- nrow(private$.nodes)
          n_new <- nrow(value)
          if ( n_new > n ){ ## node insertions
            new_ids <- setdiff(value$id, private$.nodes$id)
            if (private$.shiny_mode){
              insertNodes(id = private$.vis_id, nodes = value[value$id %in% new_ids,c("id", "x", "y"), drop=FALSE])
            } else {
              private$.vis <- insertNodes(id = private$.vis, nodes = value[value$id %in% new_ids,c("id", "x", "y"), drop=FALSE])
            }
          } else if (n_new < n){
            old_ids <- setdiff(private$.nodes$id, value$id)
            if (private$.shiny_mode){
              removeNodes(id = private$.vis_id, node_ids = old_ids)
            } else {
              private$.vis <- removeNodes(id = private$.vis, node_ids = old_ids)
            }
          }
        
          ## Node styling changes 
          changes <- get_diff(private$.nodes, value, private$.node_attr)
          # browser()
          if (length(changes) > 0){
            cols <- intersect(changes, private$.node_attr)
            if (private$.shiny_mode){
              do.call(setNodeStyle, append(list(id=private$.vis_id, node_ids=value[["id"]]), as.list(value[,cols,drop=FALSE])))
            } else {
              private$.vis <- do.call(setNodeStyle, append(list(id=private$.vis, node_ids=value[["id"]]), as.list(value[,cols,drop=FALSE])))
            }
          }
          private$.nodes <- value
          return(private$.vis)
        }
      }, 
      links = function(value){
        if (missing(value)){ return(private$.links) }
        else {
          stopifnot(is.data.frame(value))
          
          ## Link insertions / removals
          if (nrow(private$.links) != nrow(value)){
            tmp <- rbind(private$.links, value)
            changed_links <- tmp[!duplicated(tmp),]
            if ( nrow(private$.links) > nrow(value) ){ ## link removals
              if (private$.shiny_mode) {
                removeLinks(id = private$.vis_id, links=changed_links)
              } else {
                private$.vis <- removeLinks(id = private$.vis, links=changed_links)
              }
            } else if ( nrow(private$.links) < nrow(value)){
              if (private$.shiny_mode) {
                insertLinks(id = private$.vis_id, links = changed_links)
              } else {
                private$.vis <- insertLinks(id = private$.vis, links = changed_links)
              }
            }
          }
          
          ## Link style changes
          changes <- get_diff(private$.links, value, private$.link_attr)
          if (length(changes) > 0){
            cols <- intersect(changes, private$.link_attr)
            if (private$.shiny_mode){
              do.call(setLinkStyle, append(list(id=private$.vis_id, links=value[,c("source", "target")]), as.list(value[,cols,drop=FALSE])))
            } else {
              private$.vis <- do.call(setLinkStyle, append(list(id=private$.vis, links=value[,c("source", "target")]), as.list(value[,cols,drop=FALSE])))
            }
          }
          return(private$.vis)
        }
      }
    )
  )
  return(pp_active$new(pp))
}

## Package-wide environment for storing shiny options
.shiny_opts <- new.env(parent = emptyenv())

