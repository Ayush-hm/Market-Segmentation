# Loading package
library(e1071)
library(caTools)
library(class)
library(dplyr)
# Loading data
ND <- read.csv("NewData.csv")
ND$Gender[ND$Gender == "Male"] <- 0
ND$Gender[ND$Gender == "Female"] <- 1
head(ND)
# Splitting data into train
# and test data
split <- sample.split(ND, SplitRatio = 0.7)
train_cl <- subset(ND, split == "TRUE")
test_cl <- subset(ND, split == "FALSE")

# Feature Scaling
train_scale <- train_cl[, 2:5] %>% mutate(across(where(is.numeric), scale))
train_cl <- train_cl[, 2:5]
head(train_scale)
test_scale <- test_cl[, 2:5] %>% mutate(across(where(is.numeric), scale))
test_cl <-test_cl[, 2:5]
# Fitting KNN Model
# to training dataset
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Gender,
                      k = 29)
classifier_knn

# Confusiin Matrix
cm <- table(test_cl$Gender, classifier_knn)
cm

# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$Gender)
print(paste('Accuracy =', 1-misClassError))

classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Age,
                      k = 19)
classifier_knn

# Confusiin Matrix
cm <- table(test_cl$Age, classifier_knn)
cm

# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$Age)
print(paste('Accuracy =', 1-misClassError))






classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Annual_Income,
                      k = 19)
classifier_knn

# Confusiin Matrix
cm <- table(test_cl$Annual_Income, classifier_knn)
cm

# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$Annual_Income)
print(paste('Accuracy =', 1-misClassError))






classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Spending_Score,
                      k = 19)
classifier_knn

# Confusiin Matrix
cm <- table(test_cl$Spending_Score, classifier_knn)
cm

# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$Spending_Score)
print(paste('Accuracy =', 1-misClassError))