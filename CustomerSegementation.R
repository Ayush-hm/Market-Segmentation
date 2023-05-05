library(synthpop)
# reading the dataset
data = read.csv("Customers.csv")
head(data)
colnames(data)
colnames(data)[4] <- "Annual_Income"
colnames(data)[5]<-"Spending_Score"
vars=c("CustomerID","Gender","Age","Annual_Income","Spending_Score")
ods <- data[, vars]
head(ods)
# settin the seed
my.seed <- 50000
# generating the synthesis data
sds <- syn(ods, seed = my.seed)
sds
#Synthesizing data with default parametric methods is run with the methods listed below
sds_parametric <- syn(ods, method = "parametric", seed = my.seed)
sds_parametric$method
# new data generated 
new_data=sds$syn
head(new_data)
head(data)

# comparing 
compare(sds, ods, vars = "Annual_Income")
compare(sds, ods, vars = "Spending_Score")
compare(sds, ods, nrow = 3, ncol = 4, cols = vars)$plot
compare(sds, ods,  vars = vars)

write.csv(new_data,"NewData.csv", row.names = FALSE)


