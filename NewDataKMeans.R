library(ggplot2)
library(cluster)
library(dplyr)
library(factoextra)
library(cluster)
data=read.csv("NewData.csv")
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
kMeans<-kmeans(data[,3:5], 7, iter.max = 100, nstart=50)
kMeans$centers
kMeans$cluster
kMeans

clusplot(data, kMeans$cluster, color=TRUE, shade=TRUE, labels=0, lines=0)

#fviz_cluster(k7, data = data)
fviz_cluster(kMeans, data = data, choose.vars = c("Age","Annual_Income", "Spending_Score"))

# ggplot(data, aes(x = Annual_Income , y = Spending_Score)) + 
#   geom_point(stat = "identity", aes(color = as.factor(kMeans$cluster))) +
#   scale_color_discrete(name = " ", 
#                        breaks=c("1", "2", "3", "4", "5","6"),
#                        labels=c("High Income, Low Spending", "Low Income, Low Spending", "Medium Income, Medium Spending", 
#                                 "Medium Income, Medium Spending", "Low Income, High Spending","High Income, High Spending")) +
#   labs(x="Annual Income", y="Spending Score") +
#   ggtitle("Segments of Mall X Customers")


data %>%
  mutate(Cluster = kMeans$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
