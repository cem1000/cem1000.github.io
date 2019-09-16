getwd()

setwd("/Users/micha/Desktop/NCI/WebMining/CA2")

MyData <- read.csv(file="Cowles.csv", header=TRUE, sep=",")

## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

install.packages("arules")
install.packages("class", dependencies = TRUE)
install.packages("caret", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)
install.packages("e1071", dependencies = TRUE)
install.packages("FNN", dependencies = TRUE)
install.packages("gmodels", dependencies = TRUE)
install.packages("psych", dependencies = TRUE)
install.packages("data.table", dependencies=TRUE)
install.packages("BiocManager", dependecies=)
install.packages("dummies")
install.packages("ROCR")
install.packages("C50")
install.packages("irr")
install.packages("DAAG")

library("DAAG")
library("C50")
library("irr")
library("ROCR")
library("arules")
library("class")
library("caret")
library("dplyr")
library("e1071")
library("FNN")
library("gmodels")
library("psych")
library("data.table")
library("BiocManager")
library("dummies")

View(MyData)
head(MyData)

### DATA PreProcessing ###

# Check dataset contains any NAs

NAcheck <- mapply(anyNA, MyData) 
NAcheck

#No NAs shown

# check the structure of the dataframe 
str(MyData)

# drop the ID column (not relevant)
MyData$ID <- NULL
str(MyData)

# the 'sex' column is currently a factor variable, i want everything except the dependant variable to be a numeric
MyData$sex <- as.numeric(MyData$sex)
str(MyData)

# i am unsure whether the model will assume the male (2) is worth twice as much as the female.
# i will therefore change the variable into binary options
MyData$sex[MyData$sex == "2"] <- "0"
MyData$sex <- as.numeric(MyData$sex)

str(MyData)

#Run normalization on columns of dataset because the ranges of value differ across all the variables

normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))}

MyData$neuroticism<- normalize(MyData$neuroticism)
MyData$extraversion<- normalize(MyData$extraversion)
View(MyData)

# i chose to randomise the records before I divide it in two because the Yes and Nos are grouped in sequence in the dataset
MyData <- MyData[sample(nrow(MyData)),]
set.seed(1500)

# Split data into training (70%) and test (30%)
dt = sort(sample(nrow(MyData), nrow(MyData)*.7))
train<-MyData[dt,]
test<-MyData[-dt,]
View(train)

# Check number of rows in training data set
nrow(train)

### Use train() function to run k-NN

# Define controls
trctrl <- trainControl(method = "repeatedcv", 
                       number = 10, 
                       repeats = 3
                       )

# train model
knn_fit <- train(volunteer ~., data = train, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)


# to plot % accuracy vs k-value
print(knn_fit) 
plot(knn_fit, type = "b", xlab = "K- Value", ylab = "Accuracy level") 
# k = 19 is optimal k for this KNN model 

# applying the KNN model to the test data (remaining 30%) 
test_predict <- predict(knn_fit, test, type = "prob")
test_predict
table(test_predict)
pred_test <- prediction(test_predict[, 2], test$volunteer)
perf_test <- performance(pred_test, "auc")
perf_test

# ROC Curve
perf_test <- performance(pred_test, "tpr", "fpr")
plot(perf_test, col = "red", lwd = 1, main="ROC Curve for KNN")
abline(a = 0, b = 1, lwd = 3, lty = 2)

# Calculating Kolmogorov-Smirnov  statistics
ks <- max(attr(perf_test, "y.values")[[1]] - (attr(perf_test, "x.values")[[1]]))
ks

# AUC - Area Under the Curve
perf.auc <- performance(pred_test, measure="auc")
str(perf.auc)
unlist(perf.auc@y.values)
# So the AUC for the Volunteering classifier is 56% which is average

# the probability table above is not ideal for a binary answer so I will try an alternative 

test_predict2 <- predict(knn_fit, newdata = test)
test_predict2
str(test_predict2)

#we can use the confusion matrix to print results - in this case the model has returned an 56% degree of accuracy
confusionMatrix(test_predict2, test$volunteer)


#accuracy of the model is 56%
# kappa rating (ie 'correction prediction is chance') rating of 4% is also not a 'close agreement')

