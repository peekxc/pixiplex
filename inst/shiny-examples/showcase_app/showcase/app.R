library("shiny") 
library("pixiplex")

## Sample data set 
library("igraph")
g <- igraph::sample_grg(100, radius = 0)
pp <- pixiplex(g)


#plot(pp) %>% setNodeStyle(node_ids=1L, color = rgb(1, 0, 0))

ui <- fluidPage(
  title = titlePanel("Pixiplex showcase app"),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      pixiplexShinyNodeUI("node_styling_ui"),
      pixiplexShinyForceUI("force_ui")
    ), mainPanel = mainPanel(
      pixiplexShinyOutput("pp1", width = "100%", height = "50vh")
    )
  )
)

server <- function(input, output, session) {
  pp_shiny <- callModule(pixiplexShiny, "pp1", pp)
  
  ## Connect the force controller inputs to the visualization
  callModule(pixiplexShinyForce, "force_ui", pp_shiny)
  
  ## Node styling functions
  node_f <- list("Avg. filter" = function(node_ids) { apply(pp$nodes[pp$nodes$id %in% node_ids, c("x", "y")], 1, mean) })
  callModule(pixiplexShinyNode, "node_styling_ui", pp_shiny, f = node_f)
}

# Run the application 
shinyApp(ui = ui, server = server)


# htmlwidgets::saveWidget(plot(pp, canvas = TRUE), file = "pixiplex.html", selfcontained = TRUE)
# library(webshot)
# webshot("pixiplex.html", "pp1.png", selector = "#pixiplex_vis", delay = 3, debug = TRUE, useragent = "Mozilla/5.0 (Macintosh; Intel Mac OS X)")

# pp_shiny <- function(graph) {
#   rbw_pal <- substr(rainbow(10), start = 1, stop = 7)
#   shinyApp(
#     ui = fluidPage(
#       titlePanel("Testing pixiplex API"),
#       sidebarLayout(
#         sidebarPanel = sidebarPanel(
#           selectInput("pp_id", label = "Active", choices = c(1,2), selected = 1),
#           actionButton("enable_force", "Enable Force"),
#           actionButton("disable_force", "Disable Force"),
#           
#           ## Node style properties
#           sliderInput("node_radius", label = "Node Radius", min = 1, max = 25, value = 5),
#           selectInput("node_color", label = "Node Color", choices = rbw_pal, selected = rbw_pal[[1]]),
#           sliderInput("node_alpha", label = "Node Alpha", min = 0, max = 1, value = 0.8),
#           sliderInput("node_linesize", label = "Node Line size", min = 0, max = 10, value = 1),
#           selectInput("node_linecolor", label = "Node Line Color", choices = rbw_pal, selected = rbw_pal[[1]]),
#           
#           ## Link style properties
#           selectInput("link_color", label = "Link Color", choices = rbw_pal, selected = rbw_pal[[1]]),
#           sliderInput("link_linewidth", label = "Link line width", min = 1, max = 25, value = 5),
#           sliderInput("link_alpha", label = "Link Alpha", min = 0, max = 1, value = 0.8)
#           
#           ## Other
#           # verbatimTextOutput(outputId = "node_selected_out")
#         ), 
#         mainPanel = mainPanel(
#           fluidRow(
#             tabsetPanel(id="pp_tabs", 
#                         tabPanel(title = "PP1", pixiplexOutput(outputId=widget1, height="800px")), 
#                         tabPanel(title = "PP2", pixiplexOutput(outputId=widget2, height="800px"))
#             )
#           )
#         )
#       )
#     ),
#     server = function(input, output, session) {
#       ## Setup 
#       pp1 <- make_active(pixiplex(graph), id=widget1) ## make sure is in server portion
#       pp2 <- make_active(pixiplex(graph), id=widget2) ## make sure is in server portion
#       output[[widget1]] <- renderPixiplex({ pp1$plot() })
#       output[[widget2]] <- renderPixiplex({ pp2$plot() })
#       
#       ## Enable force handler
#       observeEvent(input$enable_force, { enableForce(widget_name) })
#       observeEvent(input$disable_force, { disableForce(widget_name) })
#       
#       ## Node Property handler 
#       node_inputs <- c("node_radius", "node_color", "node_alpha", "node_linesize", "node_linecolor")
#       node_outputs <- c("radius", "color", "alpha", "linesize", "linecolor")
#       observeEvent(sapply(node_inputs, function(x) input[[x]]), {
#         req(sapply(node_inputs, function(x) input[[x]]))
#         if (input$pp_id == 1){
#           pp1$nodes <- within(pp$nodes, {
#             radius = input$node_radius
#             color = input$node_color
#             alpha = input$node_alpha
#             linesize = input$node_linesize
#             linecolor = input$node_linecolor
#           })
#         } else {
#           pp2$nodes <- within(pp$nodes, {
#             radius = input$node_radius
#             color = input$node_color
#             alpha = input$node_alpha
#             linesize = input$node_linesize
#             linecolor = input$node_linecolor
#           })
#         }
#         
#         # do.call(setNodeStyle, c(list(id=widget_name, node_ids=pp$nodes$id), params))
#       })
#       
#       ## Node Selected shiny binding 
#       # observeEvent(input$node_selected, { print(input$node_selected) })
#       # output$node_selected_out <- renderText({
#       #   as.character(input$node_selected)
#       # })
#     }
#   )
# }
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white')
#     })
# }


