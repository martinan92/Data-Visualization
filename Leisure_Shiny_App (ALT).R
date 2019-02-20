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

#Load for MBD Cleaning File
load("leisure_edges.Rda")
load("leisure_key.Rda")

graph <- graph.data.frame(leisure_edges, vertices = leisure_key, directed=F)
nodes <- data.frame(id = V(graph)$name, Name = V(graph)$Full.name, Region = V(graph)$Region, 
                    Country = V(graph)$Country.of.Birth, Program = V(graph)$Academic.Program)

degree_value <- degree(graph, mode="total")
nodes$value <- degree_value[match(nodes$id, names(degree_value))]
nodes$group <- nodes$Region

edges <- leisure_edges
edges$value <- edges[,length(edges)]

############################################################################################################ 
############################################################################################################ 
############################################################################################################ 

ui <- fluidPage(
  titlePanel("Leisure Network Analysis"),
  sidebarPanel( 
    # selectInput("FILTER", "Select filter of interest", choices=c("Overall", "Region", "Country",
    #               "Program"), selected ="Overall"),
    selectInput("CONNECTION", "Select connections of interest", choices=c("Overall", "Region", "Country", 
                  "Program", "Music_Genre", "Artist", "Sport_Genre", "Athlete", "Movie_Genre", "Movie")
                , selected ="Overall")),
  visNetworkOutput("network"), 
  dataTableOutput("nodes_data_from_shiny"),
  uiOutput('dt_UI1'), 
  uiOutput('dt_UI2')
)

server <- function(input, output, session) {
  #Allows user to choose which connection type to analyze
  changing_data <- reactive({
    req(input$CONNECTION)
    selection<-c('from','to', paste(input$CONNECTION, "weight", sep="_"))
    edges<-edges[,selection]
    edges<-edges[edges[length(edges)] > 0,]
    edges$value<-edges[,length(edges)]
    edges
  })
  
 # # Allows user to choose which student groupings to analyze
 #  changing_data2 <- reactive({
 #    req(input$CONNECTION)
 # 
 #    if (input$FILTER != 'Overall'){
 #      nodes$group <- nodes[,c(input$FILTER)]
 #    }else{
 #      nodes$group <- nodes$Name
 #    }
 #    nodes
 #  })
  
  changing_data2 <- reactive({
    req(input$CONNECTION)
    
    graph <- graph.data.frame(changing_data(), vertices = nodes, directed=F)
    nodes <- data.frame(id = V(graph)$name, Name = V(graph)$Full.name, Region = V(graph)$Region, 
                        Country = V(graph)$Country.of.Birth, Program = V(graph)$Academic.Program)
    
    nodes$degree <- degree(graph, mode="total")
    nodes
  })
  
  output$network <- renderVisNetwork({
    visNetwork(nodes, changing_data(), height = "100%", width = "100%") %>%
      visOptions(highlightNearest = TRUE) %>%
      visNodes(scaling = list(min = 10, max = 50)) %>%
      visEdges(scaling = list(min = 1, max = 20)) %>%
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
  
  #Creates table of summary info of selected node
  changing_data3 <- reactive({
    req(input$CONNECTION)
    output_edges <- changing_data()
    
    #Check if node has any connections on given filter
    if (sum(myNode$selected == output_edges$from) > 0){
      connections <- output_edges[which(myNode$selected == output_edges$from),]
      top_connections <- connections[connections[length(connections)] == max(connections[length(connections)]),]
      connection_summary <- merge(top_connections, nodes, by.x = "to", by.y = "id", all.x = T)
      connection_summary[,3:ncol(connection_summary)]
    } else{
      #Otherwise return a blank table
      setNames(data.frame(matrix(ncol = 5, nrow = 0)), 
               c(colnames(output_edges[1]), colnames(nodes[,2:length(nodes)])))
    }
  })
  
  #Prints the node summary table
  output$table1 <- renderDataTable({
    nodes[which(myNode$selected == nodes$id),]
  })
  
  #Prints the connection summary table
  output$table2 <- renderDataTable({
    changing_data3()
  })
  
  output$dt_UI1 <- renderUI({
    if(nrow(nodes[which(myNode$selected == nodes$id),])!=0){
      dataTableOutput('table1')
    } else{}
  })
  
  output$dt_UI2 <- renderUI({
    if(nrow(nodes[which(myNode$selected == nodes$id),])!=0){
      dataTableOutput('table2')
    } else{}
  })
}

shiny::shinyApp(ui = ui, server = server)