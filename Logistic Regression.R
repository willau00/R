
myData <- read_excel("C:/Users/willa/Downloads/Ch9_Q17_V03_Data_File.xlsx")
View(myData) #explore data
Single <- myData$Plan
Single <- ifelse(myData$Plan == "Single",1,0) #Make plan type a binary choice
summary(Single)

#Explore with logistic regression
logisticModel <- glm(formula=Loyal ~ Age + Income + Single, family=binomial, data=myData)
summary(logisticModel)
#Make predictions with logistic regression for different hypothetical individuals
predict(logisticModel, data.frame(Age=40, Income= 80, Single=1), type= "response")
predict(logisticModel, data.frame(Age=60, Income= 80, Single=1), type = "response")

predict(logisticModel, data.frame(Age=40, Income= 80, Single=0), type= "response")
predict(logisticModel, data.frame(Age=60, Income= 80, Single=0), type = "response")



