#multi - linear
dt2 <- read.csv("Cowles_edited.csv")
str(dt2)

dt = sort(sample(nrow(dt2), nrow(dt2)*.7))
train<-dt2[dt,]
test<-dt2[-dt,]
nrow(train)

cor(dt2)

install.packages('corrplot')
library('corrplot') #package corrplot # found from this link. https://stackoverflow.com/questions/10680658/how-can-i-create-a-correlation-matrix-in-r

help("corrplot-package")

M <- cor(dt2)

corrplot(M, method = "number") #plot matrix
#this is the correlation matrix


anova1 = aov(train$volunteer ~ train$sex + train$extraversion + train$neuroticism)
summary(anova1) #test for signficance

pairs(train)
Model1 <- glm(train$volunteer ~.,family=binomial(link='logit'),data=train)
Model1 
summary(Model1)

anova(Model1, test="Chisq") #lets test for deviance

fitted.results <- predict(Model1, newdata=test, type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$volunteer)
print(paste('Accuracy',1-misClasificError))

###########"Accuracy 0.56440281030445"##############

library("ROCR")
p <- predict(Model1, newdata=test, type="response") 
p
pr <- prediction(p, test$volunteer)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

perf.auc <- performance(pr, measure="auc")
str(perf.auc)
unlist(perf.auc@y.values)


plot(auc)


a = dt2$sex #sex
b = dt2$extraversion #extraversion
c = dt2$neuroticism #neuroticism


### -1.22723  + 0.01076*x1 + 0.05812*x2 + 0.17365*x3 ### this is the equation

Model1
predicted.data <- data.frame(probability.of.volunteer=Model1$fitted.values, train=train$volunteer)
predicted.data <- predicted.data[order(predicted.data$probability.of.volunteer, decreasing = FALSE),]

#install.packages("DAAG")
#library("DAAG")

### this doesnt work cv.lm(data=train, form.lm=Model1, m= 10, plotit = F)


