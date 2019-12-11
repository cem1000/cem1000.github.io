#Loading required packages
install.packages('tidyverse')
install.packages("caret",dep = TRUE)
install.packages("ggplot2")
install.packages("lattice")
install.packages("lava")
install.packages("purrr")
install.packages("caret")
install.packages("caret",
                 repos = "http://cran.r-project.org", 
                 dependencies = c("Depends", "Imports", "Suggests"))
install.packages("lme4", dependencies = TRUE)
install.packages("pbkrtest", dependencies = TRUE)
install.packages('dplyr')
install.packages("e1071")

library(tidyverse)
library(ggplot2)
library(caret)
library(lme4)
methods(sigma)
library(caret)
library(dplyr)
library(e1071)
library(ggplot2)
library(lattice)
library(lava)
library(purrr)
library(caret)

#lets now get the wd done
getwd()
setwd("C:/Users/MyThinkpad/Desktop/Final Project/SF CRIMES!")

#;lets call the dataset (cleaned)
crime_subsetted2 <- read.csv("crimedatasubset.csv")

#lets create another subset for data only that we will use
crime_subsetted <- crime_subsetted2 %>% select(Category, DayOfWeek, Month_text, PdDistrict)

#lets make the independant a factor
crime_subsetted$Category <- factor(crime_subsetted$Category)

#Building a model
#lets now set the seed
smp_size <- floor(0.01 * nrow(crime_subsetted))
set.seed(1)

#split data into training and test data sets
test.index2 <- sample(seq_len(nrow(crime_subsetted)), size = smp_size)
train <- crime_subsetted[test.index2, ]
test <- crime_subsetted[-test.index2, ]

#Check dimensions of the split
prop.table(table(crime_subsetted$Category)) * 100
prop.table(table(train$Category)) * 100
prop.table(table(test$Category)) * 100

#create objects x which holds the predictor variables and y which holds the response variables
x = train[,-1]
y = train$Category

#lets create the model
model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))

#lets get summary
summary(model)

#Model Evaluation
#Predict testing set
Predict <- predict(model,newdata = test )
#Get the confusion matrix to see accuracy value and other parameter values

#lets get the confusion matrix!
confusionMatrix(Predict, test$Category)

####################################################


