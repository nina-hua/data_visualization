library(tidyverse)
library(maps)
library(geosphere)
library(dplyr)

# function to plot connections between cities 
plot_my_connection <- function( dep_lon, dep_lat, arr_lon, arr_lat, ...){
    inter <- gcIntermediate(c(dep_lon, dep_lat), c(arr_lon, arr_lat), n=50, addStartEnd=TRUE, breakAtDateLine=F)             
    inter=data.frame(inter)
    diff_of_lon=abs(dep_lon) + abs(arr_lon)
    if(diff_of_lon > 180){
        lines(subset(inter, lon>=0), ...)
        lines(subset(inter, lon<0), ...)
    }else{
        lines(inter, ...)
    }
}

# find connections between select cities 
# longitude and latitude 
nairobi <- c(36.8,-1.28)
geneva <- c(6.15,46.2)
cairo <- c(31.2,30.0)
dc <- c(-77.0,38.9)
newdehli <-c(77.2,28.6)

cities=rbind(
    nairobi, geneva, cairo, dc, newdehli)  %>% as.data.frame()
colnames(cities)=c("long","lat")


data <- read_csv(file = '../final/data/un_office_complete.csv')

# background map
par(mar=c(0,0,0,0))
map('world',col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05,mar=rep(0,4),border=0, ylim=c(-80,80) )

# add every connections:
plot_my_connection(dc[1], dc[2], geneva[1], geneva[2], col="orange", lwd=2)
plot_my_connection(geneva[1], geneva[2], cairo[1], cairo[2], col="orange", lwd=2)
plot_my_connection(cairo[1], cairo[2], nairobi[1], nairobi[2], col="orange", lwd=2)
plot_my_connection(nairobi[1], nairobi[2], newdehli[1], newdehli[2], col="orange", lwd=2)

# add points and names of cities
points(x=data$longitude, y=data$latitude, col="black", cex=1, pch=20)
text(c("Nairobi", "Geneva", "Cairo", "Washington D.C.", "New Delhi"), x=cities$long, y=cities$lat,  col="blue", cex=1, pos=4)
title(main = "United Nations Information Centres", line = 1)





