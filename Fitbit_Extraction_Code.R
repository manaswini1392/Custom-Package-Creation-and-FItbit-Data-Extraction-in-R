#Install the fitbitScraper package

install.packages("fitbitScraper")
library(fitbitScraper)
install.packages("ggplot2")
library(ggplot2)

setwd("C:/Users/Prani/Documents/UCONN/R/Group Project 1")

#Creating a cookie with the help of gmail id and password
df<-data.frame()
mypassword <- readLines("pw.txt") #to read text file
cookie <- login(email="palikakrishna@gmail.com", password=mypassword)

#Graphing daily steps data for a particular period of time

df <- get_daily_data(cookie, what="steps", start_date="2016-09-01", end_date="2016-09-10")

library(ggplot2)
ggplot(df) + geom_bar(aes(x=time, y=steps), stat="identity") +
  xlab("") +ylab("steps") +
  theme(axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background=element_blank(),
        panel.grid.major.y=element_line(colour="gray", size=.1),
        legend.position="none")

#Graphing distances data for a particular day using intraday_data function

df <- get_intraday_data(cookie, what="distance", date="2016-09-01")  
library("ggplot2")  
ggplot(df) + geom_bar(aes(x=time, y=distance, fill=distance), stat="identity") + 
  xlab("") +ylab("distance") + 
  theme(axis.ticks.x=element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.background=element_blank(), 
        panel.grid.major.y=element_line(colour="gray", size=.1), 
        legend.position="none") 


#To get sleep cycles data

d1<-get_sleep_data(cookie,start_date="2016-09-01", end_date="2016-09-05")

View(d1)
