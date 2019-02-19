############################################# Leisure App ################################################## 
############################################################################################################
############################################################################################################ 
############################################################################################################ 

if(!"visNetwork" %in% installed.packages()) {install.packages("visNetwork")}
library(visNetwork)
if(!"shiny" %in% installed.packages()) {install.packages("shiny")}
library(shiny)
if(!"igraph" %in% installed.packages()) {install.packages("igraph")}
library(igraph)

load("leisure_edges.Rda")
load("leisure_key.Rda")

graph <- graph.data.frame(leisure_edges, vertices = leisure_key, directed=F)
graph <- simplify(graph)

nodes <- data.frame(id = V(graph)$name, name = V(graph)$Full.name, group = V(graph)$Region, country = V(graph)$Country.of.Birth,
                    program = V(graph)$Academic.Program)
edges <- get.data.frame(graph, what="edges")[1:2]

############################################################################################################ 
############################################################################################################ 

#Show charts for the balanced list stocks using default quantmod chart settings in a Shiny App
ui <- fluidPage(
  visNetworkOutput("network"), 
  dataTableOutput("nodes_data_from_shiny"),
  uiOutput('dt_UI') 
)

server <- function(input, output, session) {
  output$network <- renderVisNetwork({
    visNetwork(nodes, edges, height = "100%", width = "100%") %>%
    visIgraphLayout(layout = "layout_in_circle") %>%
    visOptions(selectedBy = "group") %>%
    visPhysics(stabilization = F) %>%
    visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes.nodes);
                ;}")
  })
  
  myNode <- reactiveValues(selected = '')
  
  observeEvent(input$current_node_id, {
    myNode$selected <<- input$current_node_id
  })
  
  output$table <- renderDataTable({
    nodes[which(myNode$selected == nodes$id),]
  })
  
  output$dt_UI <- renderUI({
    if(nrow(nodes[which(myNode$selected == nodes$id),])!=0){
      dataTableOutput('table')
    } else{}
  })
}

shiny::shinyApp(ui = ui, server = server)
