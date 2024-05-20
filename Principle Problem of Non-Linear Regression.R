
myData <- read_excel("C:/Users/willa/Downloads/Ch8_Q25_Data_File.xlsx")
#examine plot, look for relationship
plot(GPA~Hours, data = myData)
#appears non-linear, maybe quadratic
#build a quadratic regression
quadratic <- lm(GPA~Hours + I(Hours^2), data= myData)
#now examine the R-squared term to see how well this model works
summary(quadratic)
#the R-squared and adj R-squared are good; 0.79 and 0.77
#however, examining the p-values, Hours is not significant at the 5% level. 
#I'm unsure if hours was significant in the linear model, so let's build the linear model.
linear <- lm(GPA~Hours, data= myData)
summary(linear)
#lower R-squared values, but VERY significant; very extreme t-values and P-values.
#I believe it is justified.
#Now we find where GPA is maximized. 
coeff <- coef(quadratic)
#the maximum or minimum is found at -B1/(2*B2) THIS GOES FOR QUADRATIC REGRESSIONS
-coeff[2]/(2*coeff[3])
