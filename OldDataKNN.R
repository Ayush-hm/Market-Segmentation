# Loading package
library(e1071)
library(caTools)
library(class)
library(dplyr)
# Loading data
ND <- read.csv("C:/Users/Shreya/Desktop/Data Analytics/Lab/Customers.csv")
ND$Gender[ND$Gender == "Male"] <- 0
ND$Gender[ND$Gender == "Female"] <- 1
head(ND)
colnames(ND)<-c("CustomerID","Gender","Age","AnnualIncome","SpendingScore")
# Splitting data into train
# and test data
split <- sample.split(ND, SplitRatio = 0.7)
train_cl <- subset(ND, split == "TRUE")
test_cl <- subset(ND, split == "FALSE")

# Feature Scaling
train_scale <- train_cl[, 2:5] %>% mutate(across(where(is.numeric), scale))
train_cl <- train_cl[, 2:5]
print(head(train_scale))
test_scale <- test_cl[, 2:5] %>% mutate(across(where(is.numeric), scale))
test_cl <-test_cl[, 2:5]
# Fitting KNN Model
# to training dataset
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Gender,
                      k = 29)
print(classifier_knn)

# Confusiin Matrix
cm <- table(test_cl$Gender, classifier_knn)
print(cm)

# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$Gender)
print(paste('Accuracy =', 1-misClassError))

classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Age,
                      k = 19)
print(classifier_knn)

# Confusiin Matrix
cm <- table(test_cl$Age, classifier_knn)
print(cm)

# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$Age)
print(paste('Accuracy =', 1-misClassError))


classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$SpendingScore,
                      k = 19)
print(classifier_knn)

# Confusiin Matrix
cm <- table(test_cl$SpendingScore, classifier_knn)
print(cm)

# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$SpendingScore)
print(paste('Accuracy =', 1-misClassError))



classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$AnnualIncome,
                      k = 19)
print(classifier_knn)

# Confusiin Matrix
cm <- table(test_cl$AnnualIncome, classifier_knn)
print(cm)

# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$AnnualIncome)
print(paste('Accuracy =', 1-misClassError))