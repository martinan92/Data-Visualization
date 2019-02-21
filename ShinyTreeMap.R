#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#devtools::install_github("timelyportfolio/d3treeR")
#install.packages("treemap")
library(treemap)
library(d3treeR)
library(plyr)

get_frequencies <- function(data_frame,category,percentage=TRUE) {
  if(category == 'Music') {
    list <- unlist(list(data_frame$Music.Genre0, data_frame$Music.Genre1, data_frame$Music.Genre2))
  }
  else if(category == 'Movies') {
    list <- unlist(list(data_frame$Movie.Genre0, data_frame$Movie.Genre1, data_frame$Movie.Genre2))
  }
  else if(category == 'Sports') {
    list <- unlist(list(data_frame$Sport0, data_frame$Sport1, data_frame$Sport2))
  }
  else {
    return("Error")
  }
  list <- list[list != ""]
  list <- as.data.frame(list)
  list <- count(list, 'list')
  if(percentage) {
    list$freq <- round((list$freq / sum(list$freq))*100)
  }
  colnames(list) <- c('name','freq')
  list<- list[order(list$freq, decreasing = T),]
  return(list)
}

df<-read.csv("Leisure_clean_final_raw.csv",header=TRUE,sep=",")
regions <- c('AFRICA','EUROPE','ASIAPAC','MIDDLE EAST','NORTH AMERICA','LATIN AMERICA')
genders <- c('M','F')
for(r in regions) {
  for(g in genders) {
    nam <- paste("df", r, g, sep = "_")
    assign(nam, df[df$Region == r & df$Gender == g,])
  }
}
for(c in c('Music','Movies','Sports')) {
  namm <- paste("fullList",c,sep="")
  assign(namm, data.frame()) 
  for (r in regions) {
    for(g in genders) {
      nam <- paste("df", r, g, sep = "_")
      df_temp <- get(nam)
      listName <- get_frequencies(df_temp,c,F)
      listName$region <- r
      listName$gender <- g
      assign(namm,rbind(get(namm),listName))
    }
  }
}


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Tree Map"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("Category", "Select Category", c('Music','Movies','Sports')),
         selectInput("Gender", "Filter on Gender", c('All','Male','Female')),
         selectInput("Region", "Filter on Region", c('All','ASIAPAC','AFRICA','EUROPE','MIDDLE EAST','NORTH AMERICA','LATIN AMERICA')),
         width=3
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         d3tree3Output("treeMap",width = "110%",height="650px"),
         width= 8
      )
   )
)

server <- function(input, output) {
   # renderD3tree3
   output$treeMap <- renderD3tree3({
     #toUse <- df
     if(input$Category == 'Music') {
       listToUse <- fullListMusic
     }
     else if(input$Category == 'Movies') {
       listToUse <- fullListMovies
     }
     else if(input$Category == 'Sports') {
       listToUse <- fullListSports
     }
     toUse <- listToUse
     index_ <- c(colnames(toUse)[1],colnames(toUse)[3],colnames(toUse)[4])
     if(input$Gender != 'All') {
        gender <- input$Gender
        toUse <- toUse[toUse$gender == substr(gender,1,1),]
        index_ <- c(colnames(toUse)[1],colnames(toUse)[3])
      }
     if(input$Region != 'All') {
       toUse <- toUse[toUse$region == input$Region,]
       index_ <- c(colnames(toUse)[1],colnames(toUse)[4])
     }
     if(input$Gender != 'All' & input$Region != 'All') {
       index_ <- c(colnames(toUse)[1])
     } 
      p <- treemap(toUse,index=index_,vSize=colnames(toUse)[2],fontcolor.labels = "black")
      
      name1 <- paste("Category:", input$Category, "| Gender:", input$Gender, "| Region:", input$Region, sep= " ")
      d3tree3(p, rootname = name1, celltext = "name")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

