#hw7 p3
myData <- read_excel("C:/Users/willa/Downloads/Ch11_Q31_Data_File.xlsx")
View(myData)

#replace with cutoff value of 0.25, 0.5, 0.75
yhat <- ifelse(myData$`Predicted Class 1 Probability` >= 0.25, 1, 0)
checkTP <- (yhat == myData$`Actual Class` & yhat == 1)
summary(checkTP)
TP <- sum(checkTP == 1)
print(TP)

checkTN <- (yhat == myData$`Actual Class` & yhat == 0)
summary(checkTN)
TN <- sum(checkTN == 1)
print(TN)

CheckFP <- (yhat != myData$`Actual Class` & yhat == 1)
summary(CheckFP)
FP <- sum(CheckFP == 1)
print(FP)

CheckFN <- (yhat != myData$`Actual Class` & yhat == 0)
summary(CheckFN)
FN <- sum(CheckFN == 1)
print(FN)

#Misclass Rate
misclassRate <- ((FP + FN)/(TP + TN + FP + FN))
print(misclassRate)

#Accuracy Rate
accRate <- 1 - misclassRate
print(accRate)

#Sensitivity Rate
senseRate <- (TP/(TP + FN))
print(senseRate)

#Precision Rate
precRate <- (TP/(TP + FP))
print(precRate)

#Specificity Rate
specRate <- (TN/(TN + FP))
print(specRate)

yhat <- ifelse(myData$`Predicted Class 1 Probability` >= 0.5, 1, 0)
checkTP <- (yhat == myData$`Actual Class` & yhat == 1)
summary(checkTP)
TP <- sum(checkTP == 1)
print(TP)

checkTN <- (yhat == myData$`Actual Class` & yhat == 0)
summary(checkTN)
TN <- sum(checkTN == 1)
print(TN)

CheckFP <- (yhat != myData$`Actual Class` & yhat == 1)
summary(CheckFP)
FP <- sum(CheckFP == 1)
print(FP)

CheckFN <- (yhat != myData$`Actual Class` & yhat == 0)
summary(CheckFN)
FN <- sum(CheckFN == 1)
print(FN)

#Misclass Rate
misclassRate <- ((FP + FN)/(TP + TN + FP + FN))
print(misclassRate)

#Accuracy Rate
accRate <- 1 - misclassRate
print(accRate)

#Sensitivity Rate
senseRate <- (TP/(TP + FN))
print(senseRate)

#Precision Rate
precRate <- (TP/(TP + FP))
print(precRate)

#Specificity Rate
specRate <- (TN/(TN + FP))
print(specRate)

yhat <- ifelse(myData$`Predicted Class 1 Probability` >= 0.75, 1, 0)
checkTP <- (yhat == myData$`Actual Class` & yhat == 1)
summary(checkTP)
TP <- sum(checkTP == 1)
print(TP)

checkTN <- (yhat == myData$`Actual Class` & yhat == 0)
summary(checkTN)
TN <- sum(checkTN == 1)
print(TN)

CheckFP <- (yhat != myData$`Actual Class` & yhat == 1)
summary(CheckFP)
FP <- sum(CheckFP == 1)
print(FP)

CheckFN <- (yhat != myData$`Actual Class` & yhat == 0)
summary(CheckFN)
FN <- sum(CheckFN == 1)
print(FN)

#Misclass Rate
misclassRate <- ((FP + FN)/(TP + TN + FP + FN))
print(misclassRate)

#Accuracy Rate
accRate <- 1 - misclassRate
print(accRate)

#Sensitivity Rate
senseRate <- (TP/(TP + FN))
print(senseRate)

#Precision Rate
precRate <- (TP/(TP + FP))
print(precRate)

#Specificity Rate
specRate <- (TN/(TN + FP))
print(specRate)

































yhat <- ifelse(myData$`Predicted Class 1 Probability` >= 0.5, 1, 0)
checkTP <- (yhat == myData$`Actual Class` & yhat == 1)
summary(checkTP)

checkTN <- (yhat == myData$`Actual Class` & yhat == 0)
summary(checkTN)

CheckFP <- (yhat != myData$`Actual Class` & yhat == 1)
summary(CheckFP)

CheckFN <- (yhat != myData$`Actual Class` & yhat == 0)
summary(CheckFN)



yhat <- ifelse(myData$`Predicted Class 1 Probability` >= 0.75, 1, 0)
checkTP <- (yhat == myData$`Actual Class` & yhat == 1)
summary(checkTP)

checkTN <- (yhat == myData$`Actual Class` & yhat == 0)
summary(checkTN)

CheckFP <- (yhat != myData$`Actual Class` & yhat == 1)
summary(CheckFP)

CheckFN <- (yhat != myData$`Actual Class` & yhat == 0)
summary(CheckFN)



