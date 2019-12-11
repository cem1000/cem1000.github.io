#Loading required packages
install.packages("caret",dep = TRUE)
install.packages("ggplot2")
install.packages("lattice")
install.packages("lava")
install.packages("purrr")

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



classifier = svm(formula = Category ~ ., 
                 data = train, 
                 type = 'C-classification', 
                 kernel = 'linear') 

#apply now to the test set
y_pred = predict(classifier, newdata = test[-1]) 

#confusionmatrix
confusionMatrix(y_pred, test$Category)

#Accuracy : 0.3292         
#95% CI : (0.328, 0.3305)
#No Information Rate : 0.3187         
#P-Value [Acc > NIR] : < 2.2e-16      

#Kappa : 0.0875 
