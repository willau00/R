
myData <- read_excel("C:/Users/willa/Downloads/Ch9_Q31_V05_Data_File.xlsx")
View(myData)
glmodel1 <- glm(formula = Subscribe~Discount + Age, family = binomial, data = myData)
summary(glmodel1)
#compute accuracy rate
phat1 <- predict(glmodel1, type = "response")
yhat1 <- ifelse(phat1 >= 0.5, 1, 0)
AccM1 <- 100*mean(myData$Subscribe==yhat1)
print(AccM1)


glmodel2 <- glm(formula = Subscribe~Discount + Age + Sex, family = binomial, data = myData)
summary(glmodel2)
#compute accuracy rate
phat2 <- predict(glmodel2, type = "response")
yhat2 <- ifelse(phat2 >= 0.5, 1, 0)
AccM2 <- 100*mean(myData$Subscribe==yhat2)
print(AccM2) 

predict(glmodel2, data.frame(Discount = 10, Age = 50, Sex = "Male", type = "response"))
predict(glmodel2, data.frame(Discount = 10, Age = 50, Sex = "Female", type = "response"))

