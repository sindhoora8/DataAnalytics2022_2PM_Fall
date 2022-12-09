Vector_Borne<-read.csv("Downloads/dataset.csv")
View(Vector_Borne)
install.packages("magrittr")
install.packages("dplyr")
install.packages("mapview")
library(magrittr)
library(dplyr)
library(tidyverse)
library(sf)
library(mapview)

Vector_Borne_Total<-Vector_Borne%>%filter(issue_date>='2000-01-01 00:00:00.000000') 
Vector_Borne_Total<-Vector_Borne%>%filter(cases>3) 
Vector_Borne_Total<-Vector_Borne%>%filter(cases<=100000) 


Vector_Borne_Dengue<-Vector_Borne%>%filter(disease=='dengue') 
Vector_Borne_Dengue<-Vector_Borne_Dengue%>%filter(cases>3) 
Vector_Borne_Dengue<-Vector_Borne_Dengue%>%filter(cases<=100000) 
as.numeric(Vector_Borne_Dengue$cases)
Vector_Borne_Dengue_na<-Vector_Borne_Dengue
View(Vector_Borne_Dengue)



Vector_Borne_chikungunya<-Vector_Borne%>%filter(disease=='chikungunya') 
Vector_Borne_chikungunya<-Vector_Borne_chikungunya%>%filter(issue_date>='2000-01-01 00:00:00.000000') 
as.numeric(Vector_Borne_chikungunya$cases)
Vector_Borne_chikungunya<-Vector_Borne_chikungunya%>%filter(cases>0) 
Vector_Borne_chikungunya<-Vector_Borne_chikungunya%>%filter(cases<=10000) 
Vector_Borne_chikungunya<-Vector_Borne_chikungunya%>%filter(precipitation_value>=-10) 

View(Vector_Borne_Dengue)
View(Vector_Borne_chikungunya)

install.packages("mapdata")
install.packages("mapview")
library("mapview")
library(mapdata)
help(mapview)
mapview(Vector_Borne_Total, xcol = "zoom_lon", ycol = "zoom_lat",
        crs = 4269,grid = FALSE,cex='cases',layer.name='Country'
)

mapview(Vector_Borne_Dengue, xcol = "zoom_lon", ycol = "zoom_lat",
        crs = 4269, grid = FALSE,cex='cases',layer.name='Country'
        )

mapview(Vector_Borne_chikungunya, xcol = "zoom_lon", ycol = "zoom_lat",
        crs = 4269, grid = FALSE,cex='cases',layer.name='Country'
)

plot(Vector_Borne_Dengue$cases,Vector_Borne_Dengue$precipitation_value,xlab='cases',ylab='precipitation_value')
plot(Vector_Borne_chikungunya$cases,Vector_Borne_chikungunya$precipitation_value,xlab='cases',ylab='precipitation_value')
class(Vector_Borne_Dengue$issue_date)
Vector_Borne_Dengue$issue_date 
month <- format(as.Date(Vector_Borne_Dengue$issue_date, format="%Y-%m-%d %H:%M:%OS"),"%m")
M<-as.Date(Vector_Borne_Dengue$issue_date, format="%Y-%m-%d %H:%M:%OS")
M
month
install.packages(ggplot2)
install.packages("ggplot2")
library(ggplot2)
# Basic barplot
p<-ggplot(data=Vector_Borne_Dengue, aes(y=Vector_Borne_Dengue$cases, x=format(as.Date(Vector_Borne_Dengue$issue_date, format="%Y-%m-%d %H:%M:%OS"),"%m")))+geom_bar(stat="identity")
p
p_chikungunya<-ggplot(data=Vector_Borne_chikungunya, aes(y=Vector_Borne_chikungunya$cases, x=format(as.Date(Vector_Borne_chikungunya$issue_date, format="%Y-%m-%d %H:%M:%OS"),"%m")))+geom_bar(stat="identity")
p_chikungunya

#K Means
library("ggplot2")
library("dplyr")
library("ggfortify")
View(Vector_Borne)
sqrt(399)
wssplot <- function(data, nc=19, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}
Vector_Borne_Col<-Vector_Borne %>% select("zoom_lat","zoom_lon")
wssplot(Vector_Borne_Col)
names(Vector_Borne_Dengue)
Vector_Borne_Dengue_Fil<-Vector_Borne_Col %>% select("zoom_lat","zoom_lon")
is.numeric(Vector_Borne_Dengue_Fil$issue_date)

icluster <- kmeans(Vector_Borne_Col,5)
autoplot(icluster,Vector_Borne_Dengue_Col,frame=TRUE)
fviz_cluster(icluster, data = Vector_Borne_Col, label=NA)+coord_flip()
#DB Scan 
install.packages("fpc")

# Loading package
library(fpc)
library(dbscan)
library(MASS)
library(factoextra)
library(ggh4x)
library(ggplot2)
install.packages("ggsignif")
install.packages("rstatix")
install.packages("ggsci")
library(ggsignif)
library(rstatix)
library(ggsci)

Dbscan_cl <- dbscan(Vector_Borne_Col, eps=8)
Dbscan_cl
plot(Vector_Borne_Col)
fviz_cluster(Dbscan_cl, data = Vector_Borne_Col, stand = FALSE,
             ellipse = TRUE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())


#Decision Trees
install.packages('rpart.plot')
install.packages('mlbench')
library(rpart)
library(rpart.plot)
library(mlbench)
library(tree)
install.packages('caTools')
library(caTools)
library(datasets)
library(caTools)
library(party)
library(dplyr)
library(MASS)
library(magrittr)
set.seed(1234)
Vector_Borne_Dengue_Col<-Vector_Borne_Dengue %>% select("cases","precipitation_value")
tree <- rpart(cases ~., data = Vector_Borne_Dengue_Col)
rpart.plot(tree)

#RandomForest classifaction


#Regression Model KNN
lm_Vector_Borne_Dengue<-lm(cases~precipitation_value,data=Vector_Borne_Dengue_Col)
lm_Vector_Borne_Dengue
View(Vector_Borne_Dengue_Col)
Vector_Borne_Dengue_Col<-Vector_Borne_Dengue_Col%>%filter(cases<4000) 
summary(lm_Vector_Borne_Dengue)

plot(Vector_Borne_Dengue_Col$precipitation_value,Vector_Borne_Dengue_Col$cases, pch = 16, cex = 1.3, col = "red",  ylab = "cases", xlab = "preceipitation")
abline(lm(cases~precipitation_value,data=Vector_Borne_Dengue_Col))


