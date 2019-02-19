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

############################################################################################################ 
############################################################################################################ 

ui <- fluidPage(
  titlePanel("Leisure Network Analysis"),
  sidebarPanel( 
    selectInput("FILTER", "select filter of interest", choices=c("Overall","Region", "Country", "Program"), 
                selected ="Overall")),
  visNetworkOutput("network"), 
  dataTableOutput("nodes_data_from_shiny"),
  uiOutput('dt_UI') 
)

server <- function(input, output, session) {
  
  #Overall graph
  nodes <- data.frame(id = V(graph)$name, name = V(graph)$Full.name, group = V(graph)$Region, 
                      country = V(graph)$Country.of.Birth, program = V(graph)$Academic.Program)
  edges <- leisure_edges
  
  changing_data <- reactive({
    req(input$FILTER)
    selection<-c('from','to', paste(input$FILTER, "weight", sep="_"))
    output_edges<-edges[,selection]
    output_edges<-output_edges[output_edges[length(output_edges)] > 0,]
  })
  
  output$network <- renderVisNetwork({
    visNetwork(nodes, changing_data(), height = "100%", width = "100%") %>%
    visIgraphLayout(layout = "layout_in_circle") %>%
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
