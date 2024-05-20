#hw11p4
# Example 13.8
library(readxl)
myData <- read_excel("C:/Users/willa/Downloads/Ch13_Q63_Data_File.xlsx")
#myScoreData <- read_excel("W:/Teaching/MGMT473/2023Fall/Data&Programs/ch13_Performance.xlsx", sheet = "Performance_Score")
View(myData)
library(caret)
library(gains)
library(randomForest)
library(pROC)

# 0. set Performance as binary and then partition data into 60:40 train and holdout
suppressWarnings(RNGversion("3.5.3"))
myData$Performance <- as.factor(myData$Performance)
myData$Management <- as.factor(myData$Management)
set.seed(1)
myIndex <- createDataPartition(myData$Performance, p=0.6, list=FALSE)
trainSet <- myData[myIndex,]
validationSet <- myData[-myIndex,]

# 1. develop the Bagging and Random trees and plot their feature importance.
# inputs are more important when with larger MeanDecreasexxx values.
set.seed(1)
bagging_tree <- randomForest(Performance ~ ., data = trainSet, ntree=100, mtry=3, importance=TRUE)
random_tree <- randomForest(Performance ~ ., data = trainSet, ntree=100, mtry=2, importance=TRUE)
varImpPlot(bagging_tree, type=1)
varImpPlot(random_tree, type=1)

# 2. use confusionMatrix() to check binary prediction ac curacy on the holdout w/ cutoff=.6
bagging_class <- predict(bagging_tree, validationSet)
random_class <- predict(random_tree, validationSet)
bagging_prob <- predict(bagging_tree, validationSet, type= 'prob')
random_prob <- predict(random_tree, validationSet, type= 'prob')
confusionMatrix(bagging_class, validationSet$Performance, positive = '1')
confusionMatrix(random_class, validationSet$Performance, positive = '1')
# to use cutoff=0.6:
confusionMatrix(as.factor(ifelse(bagging_prob[,2]>0.6, '1', '0')), validationSet$Performance, positive = '1')
confusionMatrix(as.factor(ifelse(random_prob[,2]>0.6, '1', '0')), validationSet$Performance, positive = '1')

# 3. create and compare the various prediction performance charts b/t bagging and random trees.
validationSet$Performance <- as.numeric(as.character(validationSet$Performance))
bagging_table <- gains(validationSet$Performance, bagging_prob[,2])
random_table <- gains(validationSet$Performance, random_prob[,2])
bagging_table
random_table
# plot and compare the two lift charts
plot(c(0, bagging_table$cume.pct.of.total*sum(validationSet$Performance)) ~ c(0, bagging_table$cume.obs), xlab = '# of cases', ylab = "Cumulative", type = "l", main="Cumulative Lift Comparison: Random Trees in Red")
lines(c(0, random_table$cume.pct.of.total*sum(validationSet$Performance)) ~ c(0, random_table$cume.obs), col="red")
lines(c(0, sum(validationSet$Performance))~c(0, dim(validationSet)[1]), col="red", lty=2)
validationSet$Performance <- as.numeric(as.character(validationSet$Performance))
#View(validationSet)
gains_table <- gains(validationSet$Performance, predicted_prob[,1])
gains_table
#plot and compare the two decile charts
barplot(bagging_table$mean.resp/mean(validationSet$Performance), names.arg=bagging_table$depth, xlab="Percentile", ylab="Lift", ylim=c(0, 3.0), main="Bagging Decile-Wise Lift Chart")
barplot(random_table$mean.resp/mean(validationSet$Performance), names.arg=random_table$depth, xlab="Percentile", ylab="Lift", ylim=c(0, 3.0), main="Random Decile-Wise Lift Chart")
# plot and compare the two ROCs and AUCs
bagging_roc <- roc(validationSet$Performance, bagging_prob[,2])
random_roc <- roc(validationSet$Performance, random_prob[,2])
plot.roc(bagging_roc, main="Bagging ROC vs. Random ROC(red)")
lines(random_roc, main="Random ROC", col="red")
auc(bagging_roc)
auc(random_roc)

