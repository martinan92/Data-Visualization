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

#Overall graph
nodes <- data.frame(id = V(graph)$name, name = V(graph)$Full.name, group = V(graph)$Region, 
                          country = V(graph)$Country.of.Birth, program = V(graph)$Academic.Program)
edges_total <- leisure_edges

#Region graph
selection<-c('from','to','Region_weight')
edges_region <- leisure_edges[selection]
edges_region<- edges_region[edges_region$Region_weight > 0,]

#Country graph
selection<-c('from','to','Country_weight')
edges_country <- leisure_edges[selection]
edges_country<- edges_country[edges_country$Country_weight > 0,]

#Program graph
selection<-c('from','to','Academic_weight')
edges_program <- leisure_edges[selection]
edges_program<- edges_program[edges_program$Program_weight > 0,]

############################################################################################################ 
############################################################################################################ 

#Show charts for the balanced list stocks using default quantmod chart settings in a Shiny App
ui <- fluidPage(
  titlePanel("sample data"),
  sidebarPanel( 
    selectInput("COUNTRY", "select country", choices=c("India","US", "UK"), selected ="India"), 
    radioButtons("MONTH","select MONTH", choices=c("jan","feb"))
  ), 
  visNetworkOutput("network"), 
  dataTableOutput("nodes_data_from_shiny"),
  uiOutput('dt_UI') 
)

server <- function(input, output, session) {
  output$network <- renderVisNetwork({
    visNetwork(nodes, edges_total, height = "100%", width = "100%") %>%
    visIgraphLayout(layout = "layout_in_circle") %>%
    visOptions(selectedBy = "group") %>%
    visPhysics(stabilization = F) %>%
    visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes.nodes);
                ;}")
  })
  
  output$network <- renderVisNetwork({
    visNetwork(nodes, edges_region, height = "100%", width = "100%") %>%
      visIgraphLayout(layout = "layout_in_circle") %>%
      visOptions(selectedBy = "group") %>%
      visPhysics(stabilization = F) %>%
      visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes.nodes);
                ;}")
  })
  output$network <- renderVisNetwork({
    visNetwork(nodes, edges_country, height = "100%", width = "100%") %>%
      visIgraphLayout(layout = "layout_in_circle") %>%
      visOptions(selectedBy = "group") %>%
      visPhysics(stabilization = F) %>%
      visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes.nodes);
                ;}")
  })
  
  output$network <- renderVisNetwork({
    visNetwork(nodes, edges_program, height = "100%", width = "100%") %>%
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
