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
    selectInput("FILTER", "Select filter of interest", choices=c("Overall", "Region", "Country", 
                "Program"), selected ="Overall"),
    selectInput("CONNECTION", "Select connections of interest", choices=c("Overall", "Region", "Country", 
                "Program", "Music_Genre", "Artist", "Sport_Genre", "Athlete", "Movie_Genre", "Movie")
                , selected ="Overall")),
  visNetworkOutput("network"), 
  dataTableOutput("nodes_data_from_shiny"),
  uiOutput('dt_UI') 
)

server <- function(input, output, session) {
  changing_data <- reactive({
    req(input$CONNECTION)
    edges <- leisure_edges
    selection<-c('from','to', paste(input$CONNECTION, "weight", sep="_"))
    output_edges<-edges[,selection]
    output_edges<-output_edges[output_edges[length(output_edges)] > 0,]
  })
  
  changing_data2 <- reactive({
    req(input$FILTER)
    nodes <- data.frame(id = V(graph)$name, Name = V(graph)$Full.name, Region = V(graph)$Region, 
                        Country = V(graph)$Country.of.Birth, Program = V(graph)$Academic.Program)
    
    if (input$FILTER != 'Overall'){
      nodes$group <- nodes[,c(input$FILTER)]
    }else{
      nodes$group <- nodes$Name
    }
    nodes
  })
  
  output$network <- renderVisNetwork({
    visNetwork(changing_data2(), changing_data(), height = "100%", width = "100%") %>%
    visIgraphLayout(layout = "layout_in_circle") %>%
    visLegend(position = "right", main = "Group") %>%
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
