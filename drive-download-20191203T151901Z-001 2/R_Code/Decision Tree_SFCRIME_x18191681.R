#decision tree algo
install.packages("dplyr")
library("dplyr")

getwd()
setwd("C:/Users/MyThinkpad/Desktop/Final Project/SF CRIMES!")

#lets call the dataset. Cleaned it beforehand using excel + R. This dataset has the top 10 most crimetypes in order increase
#accuracy
crime_subsetted2 <- read.csv("crimedatasubset.csv")


#lets create another subset for data only that we will use
crime_subsetted <- crime_subsetted2 %>% select(Category, DayOfWeek, Month_text, PdDistrict)

crime_subsetted$X = NULL

#lets just load up some machine learning algos!!
library(C50)

#lets now set the seed
smp_size <- floor(0.30 * nrow(crime_subsetted))
set.seed(1)

#splitting data in to train and test sets
test.index2 <- sample(seq_len(nrow(crime_subsetted)), size = smp_size)
crime.test <- crime_subsetted[test.index2, ]
crime.train <- crime_subsetted[-test.index2, ]

c <- C5.0Control(subset = FALSE,
                 bands = 0,
                 winnow = FALSE,
                 noGlobalPruning = FALSE,
                 CF = 0.25,
                 minCases = 2,
                 fuzzyThreshold = FALSE,
                 sample = 0,
                 seed = sample.int(4096, size = 1) -1L,
                 earlyStopping = TRUE
)

treemodel_CRIME <- C5.0(x = crime.train [, -1], y = crime.train$Category,control =c)

edit(treemodel_CRIME)

summary(treemodel_CRIME)

plot(treemodel_CRIME)


test.output <- predict(treemodel_CRIME, crime.test, type = "class")
n <- length(test.output)
number = 0
for ( i in 1:n){
  if(test.output[i] == crime.test[i, 1])
  {
    number=number+1}
}
test.accuracy = number/n*100
test.accuracy #33.17%


