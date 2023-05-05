library(ggplot2)
library(cluster)
library(dplyr)
library(factoextra)
library(cluster)
data=read.csv("Customers.csv")
str(data)
summary(data)

ggplot(data,aes(x=Age)) +
  geom_histogram()
ggplot(data,aes(x= Gender)) +
  geom_bar(stat="count",width=0.5,fill="seagreen")
set.seed(250)

#stat_gap<-clusGap(data[,2:5], FUN=kmeans, nstart=25, K.max = 10, B=50)
names(data)
stat_gap<-clusGap(data[,3:5], FUN=kmeans, nstart=25, K.max = 10, B=50)
plot(stat_gap)
#optimal clusters we can create here is 7.........
kMeans<-kmeans(data[,3:5], 6, iter.max = 100, nstart=50)
kMeans$centers
kMeans$cluster
kMeans

clusplot(data, kMeans$cluster, color=TRUE, shade=TRUE, labels=0, lines=0)
