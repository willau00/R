#hw11p2 - catergorical decision tree
suppressWarnings(RNGversion("3.5.3"))
library(readxl)
myData <- Ch13_Q13_Data_File <- read_excel("C:/Users/willa/Downloads/Ch13_Q19_Data_File.xlsx")

#View(myData)
library(caret)
library(rpart)
library(rpart.plot)

# 1. set variable of interest as binary and then partition data into 70:30 train and holdout
myData$Grad <- as.factor(myData$Grad)
set.seed(1)
myIndex <- createDataPartition(myData$Grad, p=0.7, list=FALSE)
trainSet <- myData[myIndex,]
validationSet <- myData[-myIndex,]

# 2. Optional: you may let computer to choose a default tree for practice or skip this step. 
set.seed(1)
default_tree <- rpart(Grad ~., data = trainSet, method = "class")
prp(default_tree, type = 1, extra = 1, under = TRUE)

# 3. develop the full tree and print its cp table
set.seed(1)
full_tree <- rpart(Grad ~ ., data = trainSet, method = "class", cp = 0, minsplit = 2, minbucket = 1)
printcp(full_tree)
prp(full_tree, type = 1, extra = 1, under = TRUE) #unpruned and gigantic, needs pruning

# 4.use a SLIGHTLY LARGER cp value associated with the minimum error or best pruned tree to build a pruned tree
pruned_tree <- prune(full_tree, cp = 0.0034723) #NOTICE WE INCREASED THE LAST DIGIT BY 1!!!!!!!
prp(pruned_tree, type = 1, extra = 1, under = TRUE)
printcp(pruned_tree)
# 5. use the pruned tree to predict on the holdout and validate binary accurancies
predicted_class <- predict(pruned_tree, validationSet, type = "class")
confusionMatrix(predicted_class, validationSet$Grad, positive = "1")
# this code shows how to change cutoff point to 0.1 for different predictions and accuracies
#predicted_prob <- predict(pruned_tree, validationSet, type = "prob")
#confusionMatrix(as.factor(ifelse(predicted_prob[,2]>0.1, '1', '0')), validationSet$Grad, positive = '1')

# this portion of the code creates the various prediction performance charts
library(gains)
library(pROC)
predicted_prob <- predict(pruned_tree, validationSet, type= 'prob')
head(predicted_prob)
validationSet$Grad <- as.numeric(as.character(validationSet$Grad))
#View(validationSet)
gains_table <- gains(validationSet$Grad, predicted_prob[,2])
gains_table
plot(c(0, gains_table$cume.pct.of.total*sum(validationSet$Grad)) ~ c(0, gains_table$cume.obs), xlab = '# of cases', ylab = "Cumulative", type = "l")
lines(c(0, sum(validationSet$Grad))~c(0, dim(validationSet)[1]), col="red", lty=2)
barplot(gains_table$mean.resp/mean(validationSet$Grad), names.arg=gains_table$depth, xlab="Percentile", ylab="Lift", ylim=c(0, 3.0), main="Decile-Wise Lift Chart")
roc_object <- roc(validationSet$Grad, predicted_prob[,2])
plot.roc(roc_object)
auc(roc_object) #area is larger, model is better. Max is 1.

# 6. generate binary and probability prediction on the score data
myScoreData <- read_excel("C:/Users/willa/Downloads/Ch13_Q19_Score_File.xlsx")
predicted_class_score <- predict(pruned_tree, myScoreData, type = "class")
predicted_class_score
predicted_class_prob <- predict(pruned_tree, myScoreData, type = "prob")
predicted_class_prob
