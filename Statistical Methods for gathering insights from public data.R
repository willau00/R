#Team project - supervised data mining over survey data

library(readxl)
library(caret)
library(gains)
library(pROC)
library(klaR)
library(rpart)
library(rpart.plot)
library(Metrics)
library(randomForest)


Survey <- read_excel("C:/Users/willa/Downloads/Survey.xlsx", sheet = "Clean Data")

View(Survey)
#there is a bunch of missing data in variables that might be important, so we need to fix that.

cleanSurvey <- Survey

#checking to see which columns are missing data
mean(cleanSurvey$Age)
mean(cleanSurvey$Urban) #missing values
mean(cleanSurvey$White) #missing values
mean(cleanSurvey$Male)
mean(cleanSurvey$Self_Esteem) #missing values
mean(cleanSurvey$Height) #missing values
mean(cleanSurvey$Weight) #missing values
mean(cleanSurvey$Income) #missing values

#now we clean up the missing values for columns where necessary

cleanSurvey$Urban = ifelse(is.na(cleanSurvey$Urban), median(cleanSurvey$Urban, na.rm = TRUE), cleanSurvey$Urban)

cleanSurvey$White = ifelse(is.na(cleanSurvey$White), median(cleanSurvey$White, na.rm = TRUE), cleanSurvey$White)

cleanSurvey$Self_Esteem = ifelse(is.na(cleanSurvey$Self_Esteem), median(cleanSurvey$Self_Esteem, na.rm = TRUE), cleanSurvey$Self_Esteem)

cleanSurvey$Height = ifelse(is.na(cleanSurvey$Height), median(cleanSurvey$Height, na.rm = TRUE), cleanSurvey$Height)

cleanSurvey$Weight = ifelse(is.na(cleanSurvey$Weight), median(cleanSurvey$Weight, na.rm = TRUE), cleanSurvey$Weight)

cleanSurvey$Income = ifelse(is.na(cleanSurvey$Income), median(cleanSurvey$Income, na.rm = TRUE), cleanSurvey$Income)

View(cleanSurvey)
summary(cleanSurvey)

#now the data that we care about is cleaned


#First, I'm gonna do Linear regression just to see what's up

# partition data w/ last 150 as train

TData <- cleanSurvey[3173:12686,]
VData <- cleanSurvey[1:3172,]

linear <- lm(Income ~ Age + Urban + White + Male + Self_Esteem + Height + Weight, data = TData)
summary(linear)

predT <- predict(linear, TData)
predV <-predict(linear, VData)
RMSET <- sqrt(mean((TData$Income-predT)^2))
RMSEV <- sqrt(mean((VData$Income-predV)^2))
print(RMSET)
print(RMSEV) #validation accuracy may also be a problem, there is a significant difference.


#Findings from linear regression: according to our linear regression model, 
#all variables included in the model are statistically significant, with the exception of weight.
#read the output of line 43 to see the coefficient estimates in order to determine
#the effect of each variable on Income.
#Important to note: our R^2 is only 0.07. This is super low, and we are explaining
#very little of the variation in the data.

#trying our model again without weight or the training set.

linear <- lm(Income ~ Age + Urban + White + Male + Self_Esteem + Height, data = cleanSurvey)
summary(linear)

#very similar results.

#I suspect it's possible that Age may have a quadratic effect on income;
#I would expect that as you approach 40 or 50 you earn more money each year, 
#and once you exceed 50 or so years old you begin to see less money earned on
#average each year. I will perform non-linear regression with an Age^2 term.

nlinear <- lm(Income ~ Age + I(Age^2) + Urban + White + Male + Self_Esteem + Height, data = TData)
summary(nlinear)

#The non-linear regression shows both Age and Age^2 to not be statistically significant
#as well as the intercept. Interestingly, our R^2 didn't really change at all.

#Let's try some more methods, and see if we can get a better result.

#I would suggest trying KNN and possibly a decision tree approach to see if we can 
#learn more about potential groups of individuals in the survey, and we could use
#that information to make decisions on the bank loans.


#removing outliers

cleanSurveyNOout <- subset(cleanSurvey, cleanSurvey$Income < 215000) 
summary(cleanSurveyNOout)  

linear <- lm(Income ~ Age + Urban + White + Male + Self_Esteem + Height, data = cleanSurvey)
summary(linear)

#not any better

#now I will try just removing missing values instead of imputing.



Survey1 <- read_excel("C:/Users/willa/Downloads/Survey.xlsx", sheet = "Clean Data")

cleanedSurvey <- na.omit(Survey1)

cleanSurveyShort <- subset(Survey1, Survey1$Urban == TRUE)
cleanSurveyShort <- subset(Survey1, Survey1$White == TRUE)
cleanSurveyShort <- subset(Survey1, Survey1$Self_Esteem == TRUE)
cleanSurveyShort <- subset(Survey1, Survey1$Height == TRUE)
cleanSurveyShort <- subset(Survey1, Survey1$Weight == TRUE)
cleanSurveyShort <- subset(Survey1, Survey1$Income == TRUE)
summary(cleanSurveyShort)

summary(cleanedSurvey)


cleanedSurveyNOout <- subset(cleanedSurvey, cleanedSurvey$Income < 215000) 
View(cleanedSurveyNOout) #5571 obs
557.1*7
TData <- cleanSurvey[1:3900,]
VData <- cleanSurvey[3901:5571,]


linear <- lm(Income ~ Age + Urban + White + Male + Self_Esteem + Height, data = cleanedSurveyNOout)
summary(linear)

nlinear <- lm(Income ~ Age + I(Age^2) + Urban + White + Male + Self_Esteem + Height, data = cleanedSurveyNOout)
summary(nlinear)

linear <- lm(Income ~ Age + Urban + White + Male + Self_Esteem + Height, data = TData)
summary(linear)

nlinear <- lm(Income ~ Age + I(Age^2) + Urban + White + Male + Self_Esteem + Height, data = TData)
summary(nlinear)


predT <- predict(linear, TData)
predV <-predict(linear, VData)
RMSET <- sqrt(mean((TData$Income-predT)^2))
RMSEV <- sqrt(mean((VData$Income-predV)^2))
print(RMSET) #R-squared of training set of linear model
print(RMSEV) #R-squred of validation set of linear model

#non-linear with self esteem ^2 instead of age

nlinear <- lm(Income ~ Age + I(Self_Esteem^2) + Urban + White + Male + Self_Esteem + Height, data = cleanedSurveyNOout)
summary(nlinear)

#self esteem ^2 very small, and not even significant anyway. 








#now we check out other data mining methods.

#KNN needs a binary target variable, so I'm going to look at Decision trees first.

#Example 13.7
#Import the data from the Balance_Data worksheet of the Balance data file into a data frame (table) and label it myData.
library(readxl)
myData <- cleanedSurveyNOout
#myScoreData <- read_excel("W:/Teaching/MGMT473/2023Fall/Data&Programs/ch13_Balance.xlsx", sheet ="Balance_Score")
library(caret)
library(rpart)
library(rpart.plot)
library(forecast)

# 1. partition data into 70:30 train and holdout
suppressWarnings(RNGversion("3.5.3"))
set.seed(1)
myIndex <- createDataPartition(myData$Income, p=0.7, list=FALSE)
trainSet <- myData[myIndex,]
validationSet <- myData[-myIndex,]

# 2. building the full tree and obtain cp table
set.seed(1)
full_tree <- rpart(Income ~ ., data = trainSet, method = "anova", cp = 0, minsplit = 2, minbucket = 1)
printcp(full_tree)

# 3.a-3.b prune to obtain the best pruned tree (increase the cp a little)
bp_tree <- prune(full_tree, cp = 7.5219e-02)
prp(bp_tree, type = 1, extra = 1, under = TRUE)
# prune to obtain the minimum error tree (increase the cp a little)
me_tree <- prune(full_tree, cp = 5.4877e-03)
prp(me_tree, type = 1, extra = 1, under = TRUE)

# 3.c check for best pruned tree's validation error
predicted_value <- predict(bp_tree, validationSet)
accuracy(predicted_value, validationSet$Balance)

# 3.d apply the best pruned tree to score new customers
predicted_value_score <- predict(bp_tree, myScoreData)
predicted_value_score

view(cleanedSurveyNOout)

# Example 13.5 
suppressWarnings(RNGversion("3.5.3"))
library(readxl)
myData <- cleanedSurveyNOout
#myScoreData <- read_excel("W:/Teaching/MGMT473/2023Fall/Data&Programs/ch13_HealthPlan.xlsx", sheet = "HealthPlan_Score")
View(myData)
library(caret)
library(rpart)
library(rpart.plot)

# 1. set HealthPlan as binary and then partition data into 70:30 train and holdout
myData$HealthPlan <- as.factor(myData$HealthPlan)
set.seed(1)
myIndex <- createDataPartition(myData$HealthPlan, p=0.7, list=FALSE)
trainSet <- myData[myIndex,]
validationSet <- myData[-myIndex,]

# 2. Optional: you may let computer to choose a default tree for practice or skip this step. 
set.seed(1)
default_tree <- rpart(HealthPlan ~., data = trainSet, method = "class")
prp(default_tree, type = 1, extra = 1, under = TRUE)

# 3. develop the full tree and print its cp table
set.seed(1)
full_tree <- rpart(HealthPlan ~ ., data = trainSet, method = "class", cp = 0, minsplit = 2, minbucket = 1)
printcp(full_tree)

# 4.use a SLIGHTLY LARGER cp value associated with the minimum error or best pruned tree to build a pruned tree
pruned_tree <- prune(full_tree, cp = 0.007699)
prp(pruned_tree, type = 1, extra = 1, under = TRUE)
# in this case, there is no separate best pruned tree.

# 5. use the pruned tree to predict on the holdout and validate binary accurancies
predicted_class <- predict(pruned_tree, validationSet, type = "class")
confusionMatrix(predicted_class, validationSet$HealthPlan, positive = "1")
# this code shows how to change cutoff point to 0.26 for different predictions and accuracies
# predicted_prob <- predict(pruned_tree, validationSet, type = "prob")
# confusionMatrix(as.factor(ifelse(predicted_prob[,2]>0.26, '1', '0')), validationSet$HealthPlan, positive = '1')

# this portion of the code creates the various prediction performance charts
library(gains)
library(pROC)
predicted_prob <- predict(pruned_tree, validationSet, type= 'prob')
head(predicted_prob)
validationSet$HealthPlan <- as.numeric(as.character(validationSet$HealthPlan))
gains_table <- gains(validationSet$HealthPlan, predicted_prob[,2])
gains_table
plot(c(0, gains_table$cume.pct.of.total*sum(validationSet$HealthPlan)) ~ c(0, gains_table$cume.obs), xlab = '# of cases', ylab = "Cumulative", type = "l")
lines(c(0, sum(validationSet$HealthPlan))~c(0, dim(validationSet)[1]), col="red", lty=2)
barplot(gains_table$mean.resp/mean(validationSet$HealthPlan), names.arg=gains_table$depth, xlab="Percentile", ylab="Lift", ylim=c(0, 3.0), main="Decile-Wise Lift Chart")
roc_object <- roc(validationSet$HealthPlan, predicted_prob[,2])
plot.roc(roc_object)
auc(roc_object)

# 6. generate binary and probability prediction on the score data
predicted_class_score <- predict(pruned_tree, myScoreData, type = "class")
predicted_class_score
predicted_class_prob <- predict(pruned_tree, myScoreData, type = "prob")
predicted_class_prob


##### Example 12.1
suppressWarnings(RNGversion('3.5.3'))
library(readxl)		
myData <- cleanedSurveyNOout
#myScoreData <- read_excel("W:/Teaching/MGMT473/2023Fall/Data&Programs/ch12_Gym.xlsx", sheet = "Gym_Score")
View(myData)

### 1. scaling the data
library(caret)			
myData3<-data.frame(myData$HealthPlan, myData$Age, myData$Male, myData$Self_Esteem, myData$Income, myData$Marital_Status, myData$Education, myData$WeeksEmployed)# a. b. load needed functions for KNN 
myData2<-scale(myData[,2:8])			# c. data standardization and preparation
myData1<-data.frame(myData2, myData$HealthPlan)
View(myData1)
colnames(myData1)[8]<-'HealthPlan'
myData1$HealthPlan<-as.factor(myData1$HealthPlan)
View(myData1)

###  2. partition the data and setup for kNN
set.seed(1)						
myIndex<-createDataPartition(myData1$HealthPlan,p=0.6,list=FALSE)
trainSet<-myData1[myIndex,]
validationSet<-myData1[-myIndex,]
myCtrl<-trainControl(method='cv',number=10)			# e. set for 10-fold cross validation
myGrid<-expand.grid(.k=c(1:10)) 				# f. set range of k for KNN optimization

### 3.create kNN algorithm and select optimal K
set.seed(1)							
KNN_fit<-train(HealthPlan~.,data=trainSet,method='knn',trControl=myCtrl,tuneGrid=myGrid)
KNN_fit	   # determine the optimal k=9

###  4. validate the 6NN on validation set with cutoff=0.5
# apply default cutoff=0.5 to check classification accuracy on holdout
KNN_Class <- predict(KNN_fit, newdata = validationSet)
confusionMatrix(KNN_Class, validationSet$HealthPlan, positive = '1')
#   This is just for example of predicting probability and changing cutoff value
# apply custom cutoff=0.75 to check classification accuracy on holdout
# KNN_Class_prob <- predict(KNN_fit, newdata = validationSet, type='prob')
# KNN_Class1 <- as.factor(ifelse(KNN_Class_prob[,2]>0.75, '1', '0'))
# confusionMatrix(KNN_Class1, validationSet$HealthPlan, positive = '1')

###  5. Validate the 6NN on valiation set without cutoff
library(gains)
library(pROC)
validationSet$HealthPlan <- as.numeric(as.character(validationSet$HealthPlan)) #required by R
KNN_Class_prob <- predict(KNN_fit, newdata = validationSet, type='prob')
gains_table <- gains(validationSet$HealthPlan, KNN_Class_prob[,2])
gains_table
plot(c(0, gains_table$cume.pct.of.total*sum(validationSet$HealthPlan))~c(0, gains_table$cume.obs), xlab = "# of cases", ylab = "Cumulative", main="Cumulative Lift Chart", type="l")
lines(c(0, sum(validationSet$HealthPlan))~c(0, dim(validationSet)[1]), col="red", lty=2)
barplot(gains_table$mean.resp/mean(validationSet$HealthPlan), names.arg=gains_table$depth, xlab="Percentile", ylab="Lift", ylim=c(0,3), main="Decile-Wise Lift Chart")
roc_object <- roc(validationSet$HealthPlan, KNN_Class_prob[,2])
plot.roc(roc_object)
auc(roc_object)

###  6. apply the training set statistics to score the score file
trainscale<- preProcess(myData[,2:4], method=c('center', 'scale'))    # obtain means and stds of myData columns 
myScoreData1<-predict(trainscale, myScoreData)	              # scale myScoreData with myData parameters
KNN_Score<-predict(KNN_fit,newdata=myScoreData1)	              # score myScoreData1
myScoreData<-data.frame(myScoreData,KNN_Score)
View(myScoreData)
