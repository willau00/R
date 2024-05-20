#hw11p3 - regression tree (numerical decision tree)
suppressWarnings(RNGversion("3.5.3"))

library(readxl)
myData <- Ch13_Q13_Data_File <- read_excel("C:/Users/willa/Downloads/Ch13_Q41_Data_File.xlsx")

#View(myData)
library(caret)
library(rpart)
library(rpart.plot)
library(forecast)

# 1. set variable of interest as binary and then partition data into 70:30 train and holdout
#myData$Spending <- as.numeric(myData$Spending)
set.seed(1)
myIndex <- createDataPartition(myData$Spending, p=0.7, list=FALSE)
trainSet <- myData[myIndex,]
validationSet <- myData[-myIndex,]

# 2. Optional: you may let computer to choose a default tree for practice or skip this step. 
set.seed(1)
default_tree <- rpart(Spending ~., data = trainSet, method = "anova")
prp(default_tree, type = 1, extra = 1, under = TRUE)

# 3. develop the full tree and print its cp table
set.seed(1)
full_tree <- rpart(Spending ~ ., data = trainSet, method = "anova", cp = 0, minsplit = 2, minbucket = 1)
printcp(full_tree)
prp(full_tree, type = 1, extra = 1, under = TRUE) #unpruned and gigantic, needs pruning

# 4.use a SLIGHTLY LARGER cp value associated with the minimum error or best pruned tree to build a pruned tree
pruned_tree <- prune(full_tree, cp = 1.2344e-02) #NOTICE WE INCREASED THE LAST DIGIT BY 1!!!!!!!
prp(pruned_tree, type = 1, extra = 1, under = TRUE)
printcp(pruned_tree)

#5.numeric - error checking 
predicted_value <- predict(pruned_tree, validationSet)
accuracy(predicted_value, validationSet$Spending)

# 5.catergorical - confusion matrix - use the pruned tree to predict on the holdout and validate binary accurancies
#predicted_class <- predict(pruned_tree, validationSet, type = "class")
#confusionMatrix(predicted_class, validationSet$Spending, positive = "1")
# this code shows how to change cutoff point to 0.1 for different predictions and accuracies
#predicted_prob <- predict(pruned_tree, validationSet, type = "prob")
#confusionMatrix(as.factor(ifelse(predicted_prob[,2]>0.1, '1', '0')), validationSet$Spending, positive = '1')

# this portion of the code creates the various prediction performance charts
library(gains)
library(pROC)
#predicted_prob <- predict(pruned_tree, validationSet)
#head(predicted_prob)
#validationSet$Spending <- as.numeric(as.character(validationSet$Spending))
#View(validationSet)
#gains_table <- gains(validationSet$Spending, predicted_prob[,1])
#gains_table
#plot(c(0, gains_table$cume.pct.of.total*sum(validationSet$Spending)) ~ c(0, gains_table$cume.obs), xlab = '# of cases', ylab = "Cumulative", type = "l")
#lines(c(0, sum(validationSet$Spending))~c(0, dim(validationSet)[1]), col="red", lty=2)
#barplot(gains_table$mean.resp/mean(validationSet$Spending), names.arg=gains_table$depth, xlab="Percentile", ylab="Lift", ylim=c(0, 3.0), main="Decile-Wise Lift Chart")
#roc_object <- roc(validationSet$Spending, predicted_prob[,2])
#plot.roc(roc_object)
#auc(roc_object) #area is larger, model is better. Max is 1.

# 6. generate binary and probability prediction on the score data
myScoreData <- read_excel("C:/Users/willa/Downloads/Ch13_Q41_Score_File (1).xlsx")
#predicted_class_score <- predict(pruned_tree, myScoreData, type = "class")
#predicted_class_score
predicted_value_score <- predict(pruned_tree, myScoreData)
predicted_value_score
mean(predicted_value_score)
median(predicted_value_score)
