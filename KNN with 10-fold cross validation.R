suppressWarnings(RNGversion("3.5.3"))
library(readxl)
myData <- read_excel("C:/Users/willa/Downloads/Ch12_Q1_Data_File.xlsx")
View(myData)


install.packages(c("carat", "gains", "pROC"))
library(caret)
library(gains)
library(pROC)
myData1 <- scale(myData[,2:3]) #standardize data for KNN
myData1 <- data.frame(myData1, myData$y)
View(myData1)
colnames(myData1)[3] <- "y"
myData1$y <- as.factor(myData$y)

#partition the data by automation! 60% training, 40% validation
set.seed(1)
myIndex <- createDataPartition(myData1$y, p=0.6, list=FALSE) 
TData <- myData1[myIndex,]
VData <- myData1[myIndex,]

#10 fold cross validation!

myCtrl<-trainControl(method='cv',number=10)
myGrid<-expand.grid(.k=c(1:10))

set.seed(1)
View(myData1)
View(TData)
KNN_fit <- train(y ~., data= TData, method = "knn", trControl=myCtrl, tuneGrid=myGrid)
KNN_fit

#assess the performance, build confusion matrix to test accuracy, spec, sens, etc

KNN_Class <- predict(KNN_fit, newdata = VData)
confusionMatrix(KNN_Class,VData$y, positive = '1')

KNN_Class_prob <- predict(KNN_fit, newdata = VData, type='prob')
KNN_Class_prob

confusionMatrix(as.factor(ifelse(KNN_Class_prob[,2]>0.5,'1','0')), VData$y, positive = '1')

suppressWarnings(RNGversion('3.5.3'))
library(readxl)		
myData <- read_excel("C:/Users/willa/Downloads/Ch12_Q1_Data_File.xlsx")
View(myData)

### 1. scaling the data
library(caret)			    # a. b. load needed functions for KNN 
myData1<-scale(myData[,2:3])			# c. data standardization and preparation
myData1<-data.frame(myData1, myData$y)

colnames(myData1)[3]<-'Enroll'
myData1$Enroll<-as.factor(myData1$Enroll)
View(myData1)

###  2. partition the data and setup for kNN
set.seed(1)						
myIndex<-createDataPartition(myData1$Enroll,p=0.6,list=FALSE)
trainSet<-myData1[myIndex,]
validationSet<-myData1[-myIndex,]
myCtrl<-trainControl(method='cv',number=10)			# e. set for 10-fold cross validation
myGrid<-expand.grid(.k=c(1:10)) 				# f. set range of k for KNN optimization

### 3.create kNN algorithm and select optimal K
set.seed(1)							
KNN_fit<-train(Enroll~.,data=trainSet,method='knn',trControl=myCtrl,tuneGrid=myGrid)
KNN_fit	   # determine the optimal k=6

###  4. validate the 6NN on validation set with cutoff=0.5
# apply default cutoff=0.5 to check classification accuracy on holdout
KNN_Class <- predict(KNN_fit, newdata = validationSet)
confusionMatrix(KNN_Class, validationSet$Enroll, positive = '1')
#   This is just for example of predicting probability and changing cutoff value
# apply custom cutoff=0.75 to check classification accuracy on holdout
# KNN_Class_prob <- predict(KNN_fit, newdata = validationSet, type='prob')
# KNN_Class1 <- as.factor(ifelse(KNN_Class_prob[,2]>0.75, '1', '0'))
# confusionMatrix(KNN_Class1, validationSet$Enroll, positive = '1')

###  5. Validate the 6NN on valiation set without cutoff
library(gains)
library(pROC)
validationSet$Enroll <- as.numeric(as.character(validationSet$Enroll)) #required by R
KNN_Class_prob <- predict(KNN_fit, newdata = validationSet, type='prob')
gains_table <- gains(validationSet$Enroll, KNN_Class_prob[,2])
gains_table
plot(c(0, gains_table$cume.pct.of.total*sum(validationSet$Enroll))~c(0, gains_table$cume.obs), xlab = "# of cases", ylab = "Cumulative", main="Cumulative Lift Chart", type="l")
lines(c(0, sum(validationSet$Enroll))~c(0, dim(validationSet)[1]), col="red", lty=2)
barplot(gains_table$mean.resp/mean(validationSet$Enroll), names.arg=gains_table$depth, xlab="Percentile", ylab="Lift", ylim=c(0,3), main="Decile-Wise Lift Chart")
roc_object <- roc(validationSet$Enroll, KNN_Class_prob[,2])
plot.roc(roc_object)
auc(roc_object)

myScoreData <- read_excel("C:/Users/willa/Downloads/Ch12_Q8_Score_File.xlsx")
###  6. apply the training set statistics to score the score file
trainscale<- preProcess(myData[,2:3], method=c('center', 'scale'))    # obtain means and stds of myData columns 
myScoreData1<-predict(trainscale, myScoreData)	   # scale myScoreData with myData parameters
KNN_fit
KNN_Score<-predict(KNN_fit,newdata=myScoreData1)	              # score myScoreData1
myScoreData<-data.frame(myScoreData,KNN_Score)
View(myScoreData)	

