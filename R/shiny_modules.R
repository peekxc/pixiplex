#' pixiplexShinyOutput
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @return a \code{\link{pixiplexOutput}} instance
#' @export
pixiplexShinyOutput <- function(id, width = "100%", height = "400px"){
  ns <- NS(id)
  pixiplex::pixiplexOutput(outputId = ns("pixiplex_vis"))
}

#' pixiplexShiny
#' @param pp a pixiplex object 
#' @return a reactive expression containing the pixiplex widget container id.
#' @export
pixiplexShiny <- function(input, output, session, pp){
  # print(isolate(input$pixiplex_vis))
  ns <- session$ns
  output$pixiplex_vis <- pixiplex::renderPixiplex(plot(pp))
  list(pp = reactive({ pp }), pp_vis = reactive({ ns("pixiplex_vis") }))
}

#' pixiplexShinyNodeUI
#' @export
pixiplexShinyNodeUI <- function(id, collapsible=TRUE){
  ns <- NS(id)
  node_props <- c("color", "radius", "alpha", "linesize", "linecolor")
  node_style_opts <- tagList(
    selectInput(inputId = ns("node_property"), label = "Node Property", choices = node_props, selected = head(node_props, 1)),
    uiOutput(ns("node_f")),
    actionButton(inputId = ns("node_style_button"), label = "Apply node style")
  )
  if (collapsible){
    return(shinyBS::bsCollapse(
      id = "ns_options_panel",
      shinyBS::bsCollapsePanel(title = "Node style", node_style_opts, style="margin: 0;")
    ))
  } 
  return(node_style_opts)
}

#' pixiplexShinyNode
#' @param pp reactive expression exap
#' @export
pixiplexShinyNode <- function(input, output, session, pp_shiny, f = NULL, color_pal = "rainbow"){
  if (missing(color_pal) || color_pal == "rainbow"){
    color_pal <- rev(rainbow(100, start=0, end=4/6))
  }
  # browser()
  ns <- session$ns
  if (missing(f) || is.null(f)){
    which_f <- sapply(ls(.GlobalEnv), function(symb) is.function(eval(parse(text=symb))))
    f_names <- ls(.GlobalEnv)[which_f]
    f <- structure(lapply(f_names, function(fn){ eval(parse(text=fn)) }), names=f_names)
  } 
  ## Make an input list to choose the function to apply node properties too 
  output$node_f <- renderUI({
    selectInput(inputId = ns("node_f_gen"), label = "Function", choices = names(f), selected = head(names(f), 1))
  })
  
  ## Attach observer
  observeEvent(input$node_style_button, {
    nids <- pp_shiny$pp()$nodes$id
    
    ## Evaluate the function 
    f_res <- f[[ input$node_f_gen ]](nids)
    
    ## Normalization between range
    normalize <- function(x, a = 0, b = 1){ 
      if (all(x == head(x, 1))){ return(rep(abs(b-a)/2, length(x))) }
      (b - a) * (x - min(x))/(diff(range(x))) + a
    }
    
    ## Switch behaviour based on the node property selected
    params <- list(id=pp_shiny$pp_vis(), node_ids=nids)
    params[[ input$node_property ]] <- switch(input$node_property, 
      "color" = color_pal[cut(f_res, breaks = length(color_pal), labels = FALSE)], 
      "radius" = normalize(f_res, a = 5, b = 15), 
      "alpha" = normalize(f_res, a = 0, b = 1), 
      "linesize" = normalize(f_res, a = 0, b = 10), 
      "linecolor" = color_pal[cut(f_res, breaks = length(color_pal), labels = FALSE)]
    )
    do.call(setNodeStyle, params)
  })
}

#' node_ids, color=NULL, radius=NULL, alpha=NULL, linesize=NULL, linecolor=NULL

#' pixiplexShinyForceUI
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements to control d3-force
#' @export
pixiplexShinyForceUI <- function(id, collapsible=TRUE){
  force_types <- c("forceManyBody", "forceX", "forceY", "forceRadial", "forceLink", "forceCollide", "forceCenter")
  # .shiny_opts[["force_names"]] <- c("charge", "link", "center", "custom")
  ns <- NS(id)
  assign("force_names", value = c("charge", "link", "center", "custom"), envir = .shiny_opts)
  
  
  
  force_opts <- tagList(
    checkboxInput(ns("enable_force"), label = "Enable force simulation?", value = TRUE),
    selectInput(ns("force_type"), label = "Force Type", choices = force_types, selected = head(force_types, 1)),
    checkboxInput(ns("force_enabled"), label = "Enabled?", value = TRUE, width = "45%"),
    selectInput(ns("force_name"), label = "Forces", choices = .shiny_opts[["force_names"]], selected = head(.shiny_opts[["force_names"]], 1), width = "45%"),
    conditionalPanel("input.force_name == 'custom'", 
      textInput(ns("custom_force_name"), label = "Name", value = "", placeholder = "force name"), 
      actionButton(ns("custom_force_button"), label = "Add force"), 
      ns = ns
    ),
    conditionalPanel("input.force_type == 'forceManyBody'", 
      sliderInput(ns("fmb_strength"), label = "Force Strength", min = -100, max = 100, value = -30, step = 1L), 
      sliderInput(ns("fmb_theta"), label = "Force Theta", min = 0.1, max = 1, value = 0.9, step = 0.05), 
      numericInput(ns("fmb_distance_min"), label = "Distance min", value = 1, min = 1),
      numericInput(ns("fmb_distance_min"), label = "Distance max", value = Inf, min = 1), 
      ns = ns
    ), 
    conditionalPanel("input.force_type == 'forceX'",
      sliderInput(ns("fx_strength"), label = "Force Strength", min = 0, max = 1, value = 0.1, step = 0.01),
      numericInput(ns("fx_x"), label = "X", value = 0), 
      ns = ns
    ), 
    conditionalPanel("input.force_type == 'forceY'",
      sliderInput(ns("fy_strength"), label = "Force Strength", min = 0, max = 1, value = 0.1, step = 0.01),
      numericInput(ns("fy_y"), label = "Y", value = 0), 
      ns = ns
    ), 
    conditionalPanel("input.force_type == 'forceRadial'",
      sliderInput(ns("fr_strength"), label = "Force Strength", min = 0, max = 1, value = 0.1, step = 0.01),
      numericInput(ns("fr_x"), label = "X", value = 0),
      numericInput(ns("fr_y"), label = "Y", value = 0), 
      numericInput(ns("fr_radius"), label = "Radius", value = 1), 
      ns = ns
    ), 
    conditionalPanel("input.force_type == 'forceLink'", 
      sliderInput(ns("fl_distance"), label = "Link distance", min = 0, max = 300, value = 30, step = 1),        
      sliderInput(ns("fl_strength"), label = "Link Strength", min = 0, max = 1, value = 0.1, step = 0.01),
      sliderInput(ns("fl_iterations"), label = "Link Iterations", min = 1, max = 100, value = 1, step = 1), 
      ns = ns
    ), 
    conditionalPanel("input.force_type == 'forceCollide'",
      numericInput(ns("fc_radius"), label = "Collide radius", min = 1, value = 1),        
      sliderInput(ns("fc_strength"), label = "Collide Strength", min = 0, max = 1, value = 0.7, step = 0.01),
      sliderInput(ns("fc_iterations"), label = "Collide Iterations", min = 1, max = 100, value = 1, step = 1), 
      ns = ns
    ), 
    conditionalPanel("input.force_type == 'forceCenter'",
      numericInput(ns("fcenter_x"), label = "X", value = 0),
      numericInput(ns("fcenter_y"), label = "Y", value = 0), 
      ns = ns
    ), 
    actionButton(ns("apply_force"), "Apply Force")
  )
  if (collapsible){
    return(shinyBS::bsCollapse(
      id = "force_options_panel",
      shinyBS::bsCollapsePanel(title = "Force Settings", force_opts, style="margin: 0;")
    ))
  } 
  return(force_opts)
}

#' pixiplexShinyForce
#' @param input, output, session standard \code{shiny} boilerplate
#' @param pp_shiny list containing reactive expressions for the pixiplex object and the container its associated with.
#' @return None.
#' @export
pixiplexShinyForce <- function(input, output, session, pp_shiny){
  
  ## If action button pressed, add the force to the list of choices 
  # observeEvent(input$custom_force_button, {
  #   updateSelectInput(session = session, choices = .shiny_opts[["force_names"]])
  # })
  
  ## On force application button press, call 'setForce' w/ current visualization
  observeEvent(input$apply_force, {
    req(input$enable_force)
    do.call(ifelse(input$enable_force, enableForce, disableForce), list(id=pp_shiny$pp_vis()))
    init <- list(id=pp_shiny$pp_vis(), name=input$force_name, enabled=input$force_enabled, type=input$force_type)
    params <- switch(input$force_type,
      "forceManyBody" = list(strength=input$fmb_strength, theta=input$fmb_theta, distanceMin=input$fmb_distance_min, distanceMax=input$distance_max), 
      "forceX" = list(strength=input$fx_strength, x=input$fx_x),
      "forceY" = list(strength=input$fy_strength, y=input$fy_y), 
      "forceRadial" = list(strength=input$fr_strength, radius=input$fr_radius, x=input$fr_x, y=input$fr_x), 
      "forceLink" = list(distance=input$fl_distance, strength=input$fl_strength, iterations=input$fl_iterations),
      "forceCollide" = list(distance=input$fc_distance, strength=input$fc_strength, iterations=input$fc_iterations), 
      "forceCenter" = list(x=input$fcenter_x, y=input$fcenter_y)
    )
    do.call(setForce, modifyList(init, params))
  })
  
  ## Update the list to reflect the force type and associated parameters
  observe(input$force_name, {
    
  })
}
