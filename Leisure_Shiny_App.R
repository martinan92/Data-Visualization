############################################# Leisure App ################################################## 
############################################################################################################
############################################################################################################ 
############################################################################################################ 

# install dev version
devtools::install_github("datastorm-open/visNetwork")

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

nodes <- data.frame(id = V(graph)$name, title = V(graph)$name, group = V(graph)$Academic.Program.1)
#nodes <- data.frame(id = V(graph)$name, title = V(graph)$name, group = V(graph)$Country.of.Birth)
edges <- get.data.frame(graph, what="edges")[1:2]


############################################################################################################ 
############################################################################################################ 

#Show charts for the balanced list stocks using default quantmod chart settings in a Shiny App
ui <- fluidPage(
  visNetworkOutput("network"), 
  verbatimTextOutput("shiny_return")  
)

server <- function(input, output, session) {
  output$network <- renderVisNetwork({
    visNetwork(nodes, edges, height = "100%", width = "100%") %>%
    visIgraphLayout(layout = "layout_in_circle") %>%
    #visOptions(highlightNearest = list(enabled = T, hover = T), nodesIdSelection = T) %>%
    visOptions(selectedBy = "group") %>%
    visLegend(position = "right", main = "Group") %>%
    visEvents(click = "function(nodes){
              Shiny.onInputChange('click', nodes.nodes[0]);
              ;}")
  
  })
  output$shiny_return <- renderPrint({
    visNetworkProxy("network") %>%
      visNearestNodes(target = input$click)
  })
}

shiny::shinyApp(ui = ui, server = server)
