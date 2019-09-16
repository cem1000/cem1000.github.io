Cowles$X<-NULL
Firstly, the X column which was a list of all the observations was removed for the data so that it would not influence the decision tree.

# Make dependent variable as a factor (categorical)
Cowles$sex<-as.factor(Cowles$sex)
Cowles$volunteer<-as.factor(Cowles$volunteer)

To make the model a correct fit for the model the dependent variable 'volunteer' needed to be changed to a categorical factor. 

str(Cowles)
summary(Cowles)

# Split data into training (70%) and validation (30%)
Cow <- sort(sample(nrow(Cowles), nrow(Cowles)*.7))
train<-Cowles[Cow,]
validate<-Cowles[-Cow,] # Check number of rows in training data set
nrow(train)
nrow(validate)

Then the dataset needed to be partitioned into test and training data. Using a standard method of 70/30 split. 

# Decision Tree Model
library(rpart)
mtree <- rpart(volunteer~., data = train, method="class", control = rpart.control(minsplit = 20, minbucket = 7, maxdepth = 10, usesurrogate = 2, xval =10 ))
Mtree


par(mfrow=c(1,1))
plot(mtree)
text(mtree)

The decision tree model yielded the following result when applied to the neuroticism ~ extraversion independant variables:
  
  
  
  The tree suggests that peoplle 


#Beautify tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)


#view1
prp(mtree, faclen = 0, cex = 0.8, extra = 1)



#view2 - total count at each node
tot_count <- function(x, labs, digits, varlen)
{paste(labs, "\n\nn =", x$frame$n)}

prp(mtree, faclen = 0, cex = 0.8, node.fun=tot_count)

#view3- fancy Plot
library(rattle)
#library(gKt)
#rattle()
fancyRpartPlot(mtree)



printcp(mtree)

CP or Complexity Parameter 



#####################################################################
bestcp <- mtree$cptable[which.min(mtree$cptable[,"xerror"]),"CP"]

# Prune the tree using the best cp.
pruned <- prune(mtree, cp = bestcp)
prp(pruned, faclen = 0, cex = 0.8, extra = 1)
#####################################################################

# confusion matrix (training data)
conf.matrix <- table(train$volunteer, predict(pruned,type="class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)


#Scoring
library(ROCR)
val1 = predict(pruned, validate, type = "prob")
val1
#Storing Model Performance Scores
pred_val <-prediction(val1[,2],validate$volunteer)
pred_val
# Calculating Area under Curve
perf_val <- performance(pred_val,measure = "auc")#area under curve
Perf_val

unlist(perf_val@y.values)
# Plotting Lift curve
plot(performance(pred_val, measure="lift", x.measure="rpp"), colorize=TRUE)

# Calculating True Positive and False Positive Rate
perf_val <- performance(pred_val, "tpr", "fpr")
# Plot the ROC curve
plot(perf_val, col = "green", lwd = 1.5)
abline(a=0,b=1,lwd=3,lty=2)
#Calculating KS statistics
ks1.tree <- max(attr(perf_val, "y.values")[[1]] - (attr(perf_val, "x.values")[[1]]))
ks1.tree

# Advanced Plot
prp(pruned, main="Volunteer",
    extra=106,
    nn=TRUE,
    fallen.leaves=TRUE,
    branch=.5,
    faclen=0,
    trace=1,
    shadow.col="red",
    branch.lty=3,
    split.cex=1.2,
    split.prefix="is ",
    split.suffix="?",
    split.box.col="lightgray",
    split.border.col="darkgray",
    split.round=.8)







