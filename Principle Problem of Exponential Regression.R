
myData <- read_excel("C:/Users/willa/Downloads/Ch8_Q33_Data_File.xlsx")
View(myData)
#build the linear model
linFit <- lm(Rent~Beds + Baths + Sqft, data=myData)
summary(linFit)
#predicting the rent using the linear model is easy:
predict(linFit, data.frame(Beds=3,Baths=2,Sqft=1500))
#linear model has decent R-sqared values of 0.80, but only Baths is statisically significant.
#now we need to build an exponential model
expFit <- lm(log(Rent)~Beds + Baths + Sqft, data=myData)
summary(expFit)
#predicting the rent using the exponential model is harder:
#first, we get the estimation of ln(rent):
plog3 <- predict(expFit, data.frame(Beds=3,Baths=2,Sqft=1500))
#then we need the standard error
se <- sigma(expFit)
#then we adjust our estimation using this equation:
exp(plog3 + se^2/2)
#we want to find the R^2 of our exponential model using ONLY Rent, not ln(Rent)
#to do this, we need to take out the ln, without destroying the model.
#here is how we find R^2 of our exponential model:
Pred_lnRent <- predict(expFit)
Pred_Rent <- exp(Pred_lnRent+se^2/2)
cor(myData$Rent, Pred_Rent)^2
