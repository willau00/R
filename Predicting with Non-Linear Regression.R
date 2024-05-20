
myData <- read_excel("C:/Users/willa/Downloads/Ch7_Q29_Data_File.xlsx")
View(myData)
#examine data
plot(Cost~Quantity, data = myData)
#definitely not linear; it's U shaped; quadratic
#making the linear model as asked in part 1.
linFit <- lm(Cost~Quantity, data = myData)
#the coefficient on Quantity is essentially 0, so see if it's statistically significant.
summary(linFit)
#VERY low R-squared, sure enough quantity is not statistically significant.
#lets make a quadratic model...
quadFit <- lm(Cost~Quantity + I(Quantity^2), data = myData)
summary(quadFit)
#very good R-squared value; 0.86, all very statistically significant. 
#predict the cost given the quantity of 800 units using the quadratic model
predict(quadFit, data.frame(Quantity = 800))
