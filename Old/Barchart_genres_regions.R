library(ggplot2)
library(scales)
library(tidyr)
library(gdata)
library(dplyr)

music<-read.xls("genres.xlsx",sheet = 1,header=TRUE)

data_music<- gather(music, music_type, Percent, -region)


ggplot(data_music , aes(music_type,Percent)) + 
  geom_bar(aes(fill = region), stat = "identity", position = "dodge")+ 
  scale_y_continuous(labels = scales::percent)+
  scale_y_continuous(labels = percent_format(accuracy = 2))+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(title="Percentage of Top 3 Music Genre among Regions")+ 
  theme(legend.title = element_blank())+
  xlab("")



sport<-read.xls("genres.xlsx",sheet = 2,header=TRUE)

data_sport<- gather(sport, sport_type, Percent, -region)

ggplot(data_sport, aes(sport_type,Percent)) + 
  geom_bar(aes(fill = region), stat = "identity", position = "dodge")+ 
  scale_y_continuous(labels = scales::percent)+
  scale_y_continuous(labels = percent_format(accuracy = 2))+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(title="Percentage of Top 3 Sport among Regions")+ 
  theme(legend.title = element_blank())+
  xlab("")



movie<-read.xls("genres.xlsx",sheet = 3,header=TRUE)

data_movie <- gather(movie, movie_type, Percent, -region)

ggplot(data_movie, aes(movie_type,Percent)) + 
  geom_bar(aes(fill = region), stat = "identity", position = "dodge")+ 
  scale_y_continuous(labels = scales::percent)+
  scale_y_continuous(labels = percent_format(accuracy = 2))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(title="Percentage of Top 3 Movie Type among Regions")+ 
  theme(legend.title = element_blank())+
  xlab("")
