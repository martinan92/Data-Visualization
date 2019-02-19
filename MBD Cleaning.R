data<-read.csv("Leisure_clean_final_raw.csv",header=TRUE,sep=",")

########################################################################################################
########################################################################################################
########################################################################################################
Academic_weight = vector()
from = vector()
to = vector()

for (first in 1:nrow(data)){
  for (second in 1:nrow(data)){
    condition1 <- (data[first,c('Academic.Program')] == data[second,c('Academic.Program')] &
                     data[first,c('Academic.Program')] != "")

    if (first != second){
      from <- append(from, first)
      to <- append(to, second)
      Academic_weight <- append(Academic_weight, sum(condition1))
    }
  }
}

########################################################################################################
########################################################################################################
########################################################################################################
Country_weight = vector()

for (first in 1:nrow(data)){
  for (second in 1:nrow(data)){
    condition <- (data[first,c('Country.of.Birth')] == data[second,c('Country.of.Birth')]& 
                    data[first,c('Country.of.Birth')] != "")
    if (first != second){
      Country_weight <- append(Country_weight, sum(condition))
    }
  }
}

########################################################################################################
########################################################################################################
########################################################################################################
Region_weight = vector()

for (first in 1:nrow(data)){
  for (second in 1:nrow(data)){
    condition <- (data[first,c('Region')] == data[second,c('Region')]& 
                    data[first,c('Region')] != "")
    if (first != second){
      Region_weight <- append(Region_weight, sum(condition))
    }
  }
}

########################################################################################################
########################################################################################################
########################################################################################################
Music_Genre_weight = vector()

for (first in 1:nrow(data)){
  for (second in 1:nrow(data)){
    condition1 <- (data[first,c('Music.Genre0')] == data[second,c('Music.Genre0')]& 
                     data[first,c('Music.Genre0')] != "")
    condition2 <- (data[first,c('Music.Genre0')] == data[second,c('Music.Genre1')]& 
                     data[first,c('Music.Genre0')] != "") 
    condition3 <- (data[first,c('Music.Genre0')] == data[second,c('Music.Genre2')]& 
                     data[first,c('Music.Genre0')] != "")
    condition4 <- (data[first,c('Music.Genre1')] == data[second,c('Music.Genre0')]& 
                     data[first,c('Music.Genre1')] != "")
    condition5 <- (data[first,c('Music.Genre1')] == data[second,c('Music.Genre1')]& 
                     data[first,c('Music.Genre1')] != "")
    condition6 <- (data[first,c('Music.Genre1')] == data[second,c('Music.Genre2')]& 
                     data[first,c('Music.Genre1')] != "")
    condition7 <- (data[first,c('Music.Genre2')] == data[second,c('Music.Genre0')]& 
                     data[first,c('Music.Genre2')] != "")
    condition8 <- (data[first,c('Music.Genre2')] == data[second,c('Music.Genre1')]& 
                     data[first,c('Music.Genre2')] != "")
    condition9 <- (data[first,c('Music.Genre2')] == data[second,c('Music.Genre2')]& 
                     data[first,c('Music.Genre2')] != "")
    
    if (first != second){
      Music_Genre_weight <- append(Music_Genre_weight, sum(condition1,condition2,condition3,condition4
                                            ,condition5,condition6,condition7,condition8,condition9))
    }
  }
}

########################################################################################################
########################################################################################################
########################################################################################################
Artist_weight = vector()

for (first in 1:nrow(data)){
  for (second in 1:nrow(data)){
    condition1 <- (data[first,c('Artist0')] == data[second,c('Artist0')]& 
                     data[first,c('Artist0')] != "")
    condition2 <- (data[first,c('Artist0')] == data[second,c('Artist1')]& 
                     data[first,c('Artist0')] != "") 
    condition3 <- (data[first,c('Artist0')] == data[second,c('Artist2')]& 
                     data[first,c('Artist0')] != "")
    condition4 <- (data[first,c('Artist1')] == data[second,c('Artist0')]& 
                     data[first,c('Artist1')] != "")
    condition5 <- (data[first,c('Artist1')] == data[second,c('Artist1')]& 
                     data[first,c('Artist1')] != "")
    condition6 <- (data[first,c('Artist1')] == data[second,c('Artist2')]& 
                     data[first,c('Artist1')] != "")
    condition7 <- (data[first,c('Artist2')] == data[second,c('Artist0')]& 
                     data[first,c('Artist2')] != "")
    condition8 <- (data[first,c('Artist2')] == data[second,c('Artist1')]& 
                     data[first,c('Artist2')] != "")
    condition9 <- (data[first,c('Artist2')] == data[second,c('Artist2')]& 
                     data[first,c('Artist2')] != "")
    
    ranking_weight <- 0
    if (condition1){
      ranking_weight <- ranking_weight + 3
    }else if (condition5){
      ranking_weight <- ranking_weight + 2
    }else if (condition9){
      ranking_weight <- ranking_weight + 1
    }
    
    if (first != second){
      Artist_weight <- append(Artist_weight, ranking_weight + sum(condition1,condition2,condition3
                                    ,condition4,condition5,condition6,condition7,condition8,condition9) * 2)
    }
  }
}

########################################################################################################
########################################################################################################
########################################################################################################
Sport_Genre_weight = vector()

for (first in 1:nrow(data)){
  for (second in 1:nrow(data)){
    condition1 <- (data[first,c('Sport0')] == data[second,c('Sport0')]& 
                     data[first,c('Sport0')] != "")
    condition2 <- (data[first,c('Sport0')] == data[second,c('Sport1')]& 
                     data[first,c('Sport0')] != "") 
    condition3 <- (data[first,c('Sport0')] == data[second,c('Sport2')]& 
                     data[first,c('Sport0')] != "")
    condition4 <- (data[first,c('Sport1')] == data[second,c('Sport0')]& 
                     data[first,c('Sport1')] != "")
    condition5 <- (data[first,c('Sport1')] == data[second,c('Sport1')]& 
                     data[first,c('Sport1')] != "")
    condition6 <- (data[first,c('Sport1')] == data[second,c('Sport2')]& 
                     data[first,c('Sport1')] != "")
    condition7 <- (data[first,c('Sport2')] == data[second,c('Sport0')]& 
                     data[first,c('Sport2')] != "")
    condition8 <- (data[first,c('Sport2')] == data[second,c('Sport1')]& 
                     data[first,c('Sport2')] != "")
    condition9 <- (data[first,c('Sport2')] == data[second,c('Sport2')]& 
                     data[first,c('Sport2')] != "")
    
    if (first != second){
      Sport_Genre_weight <- append(Sport_Genre_weight, sum(condition1,condition2,condition3,condition4
                                                ,condition5,condition6,condition7,condition8,condition9))
    }
  }
}

########################################################################################################
########################################################################################################
########################################################################################################
Athlete_weight = vector()

for (first in 1:nrow(data)){
  for (second in 1:nrow(data)){
    condition1 <- (data[first,c('Athlete0')] == data[second,c('Athlete0')]& 
                     data[first,c('Athlete0')] != "")
    condition2 <- (data[first,c('Athlete0')] == data[second,c('Athlete1')]& 
                     data[first,c('Athlete0')] != "") 
    condition3 <- (data[first,c('Athlete0')] == data[second,c('Athlete2')]& 
                     data[first,c('Athlete0')] != "")
    condition4 <- (data[first,c('Athlete1')] == data[second,c('Athlete0')]& 
                     data[first,c('Athlete1')] != "")
    condition5 <- (data[first,c('Athlete1')] == data[second,c('Athlete1')]& 
                     data[first,c('Athlete1')] != "")
    condition6 <- (data[first,c('Athlete1')] == data[second,c('Athlete2')]& 
                     data[first,c('Athlete1')] != "")
    condition7 <- (data[first,c('Athlete2')] == data[second,c('Athlete0')]& 
                     data[first,c('Athlete2')] != "")
    condition8 <- (data[first,c('Athlete2')] == data[second,c('Athlete1')]& 
                     data[first,c('Athlete2')] != "")
    condition9 <- (data[first,c('Athlete2')] == data[second,c('Athlete2')]& 
                     data[first,c('Athlete2')] != "")
    
    ranking_weight <- 0
    if (condition1){
      ranking_weight <- ranking_weight + 3
    }else if (condition5){
      ranking_weight <- ranking_weight + 2
    }else if (condition9){
      ranking_weight <- ranking_weight + 1
    }
    
    if (first != second){
      Athlete_weight <- append(Athlete_weight, ranking_weight + sum(condition1,condition2,condition3
                                  ,condition4,condition5,condition6,condition7,condition8,condition9) * 2)
    }
  }
}

########################################################################################################
########################################################################################################
########################################################################################################
Movie_Genre_weight = vector()

for (first in 1:nrow(data)){
  for (second in 1:nrow(data)){
    condition1 <- (data[first,c('Movie.Genre0')] == data[second,c('Movie.Genre0')]& 
                     data[first,c('Movie.Genre0')] != "")
    condition2 <- (data[first,c('Movie.Genre0')] == data[second,c('Movie.Genre1')]& 
                     data[first,c('Movie.Genre0')] != "") 
    condition3 <- (data[first,c('Movie.Genre0')] == data[second,c('Movie.Genre2')]& 
                     data[first,c('Movie.Genre0')] != "")
    condition4 <- (data[first,c('Movie.Genre1')] == data[second,c('Movie.Genre0')]& 
                     data[first,c('Movie.Genre1')] != "")
    condition5 <- (data[first,c('Movie.Genre1')] == data[second,c('Movie.Genre1')]& 
                     data[first,c('Movie.Genre1')] != "")
    condition6 <- (data[first,c('Movie.Genre1')] == data[second,c('Movie.Genre2')]& 
                     data[first,c('Movie.Genre1')] != "")
    condition7 <- (data[first,c('Movie.Genre2')] == data[second,c('Movie.Genre0')]& 
                     data[first,c('Movie.Genre2')] != "")
    condition8 <- (data[first,c('Movie.Genre2')] == data[second,c('Movie.Genre1')]& 
                     data[first,c('Movie.Genre2')] != "")
    condition9 <- (data[first,c('Movie.Genre2')] == data[second,c('Movie.Genre2')]& 
                     data[first,c('Movie.Genre2')] != "")
    
    if (first != second){
      Movie_Genre_weight <- append(Movie_Genre_weight, sum(condition1,condition2,condition3,condition4
                                                ,condition5,condition6,condition7,condition8,condition9))
    }
  }
}

########################################################################################################
########################################################################################################
########################################################################################################
Movie_weight = vector()

for (first in 1:nrow(data)){
  for (second in 1:nrow(data)){
    condition1 <- (data[first,c('Movie0')] == data[second,c('Movie0')]& 
                     data[first,c('Movie0')] != "")
    condition2 <- (data[first,c('Movie0')] == data[second,c('Movie1')]& 
                     data[first,c('Movie0')] != "") 
    condition3 <- (data[first,c('Movie0')] == data[second,c('Movie2')]& 
                     data[first,c('Movie0')] != "")
    condition4 <- (data[first,c('Movie1')] == data[second,c('Movie0')]& 
                     data[first,c('Movie1')] != "")
    condition5 <- (data[first,c('Movie1')] == data[second,c('Movie1')]& 
                    data[first,c('Movie1')] != "")
    condition6 <- (data[first,c('Movie1')] == data[second,c('Movie2')]& 
                     data[first,c('Movie1')] != "")
    condition7 <- (data[first,c('Movie2')] == data[second,c('Movie0')]& 
                     data[first,c('Movie2')] != "")
    condition8 <- (data[first,c('Movie2')] == data[second,c('Movie1')]& 
                     data[first,c('Movie2')] != "")
    condition9 <- (data[first,c('Movie2')] == data[second,c('Movie2')]& 
                     data[first,c('Movie2')] != "")
    
    ranking_weight <- 0
    if (condition1){
      ranking_weight <- ranking_weight + 3
    }else if (condition5){
      ranking_weight <- ranking_weight + 2
    }else if (condition9){
      ranking_weight <- ranking_weight + 1
    }
    
    if (first != second){
      Movie_weight <- append(Movie_weight, ranking_weight + sum(condition1,condition2,condition3,condition4
                                      ,condition5,condition6,condition7,condition8,condition9) * 2)
    }
  }
}

########################################################################################################
########################################################################################################
########################################################################################################

leisure_edges <- data.frame(from)
leisure_edges <- cbind(leisure_edges,to)
leisure_edges <- cbind(leisure_edges,Academic_weight)
leisure_edges <- cbind(leisure_edges,Country_weight)
leisure_edges <- cbind(leisure_edges,Region_weight)
leisure_edges <- cbind(leisure_edges,Music_Genre_weight)
leisure_edges <- cbind(leisure_edges,Artist_weight)
leisure_edges <- cbind(leisure_edges,Sport_Genre_weight)
leisure_edges <- cbind(leisure_edges,Athlete_weight)
leisure_edges <- cbind(leisure_edges,Movie_Genre_weight)
leisure_edges <- cbind(leisure_edges,Movie_weight)

#Exclude Academic and Country for total weight
leisure_edges$total_weight <- rowSums(leisure_edges[,5:ncol(leisure_edges)])

#Remove connections of 0 aggregate weight
leisure_edges <-leisure_edges[leisure_edges$total_weight!=0, ] 
leisure_key <- data

head(leisure_edges)
head(leisure_key)

save(leisure_edges,file="leisure_edges.Rda")
save(leisure_key,file="leisure_key.Rda")
