
myData <- read_excel("C:/Users/willa/Downloads/Ch11_Q35_V04_Data_File.xlsx")
View(myData)
#errors
err1 <- myData$`Actual Spending` - myData$`Predicted Spending 1` 
err2 <- myData$`Actual Spending` - myData$`Predicted Spending 2` 

#ME
ME1 <- mean(err1)ME2 <- mean(err2)
#RMSE
err1sq <- err1^2
RMSE1 <- sqrt(mean(err1sq))
err2sq <- err2^2
RMSE2 <- sqrt(mean(err2sq))
#MAD
abserr1 <- abs(err1)
MAD1 <- mean(abserr1)
abserr2 <- abs(err2)
MAD2 <- mean(abserr2)
#MPE
errpct1 <- err1/myData$`Actual Spending`
MPE1 <- mean(errpct1) *100
errpct2 <- err2/myData$`Actual Spending`
MPE2 <- mean(errpct2) *100
#MAPE
abserrpct1 <- abs(errpct1)
MAPE1 <- mean(abserrpct1)*100
abserrpct2 <- abs(errpct2)
MAPE2 <- mean(abserrpct2)*100

#base model

#RMSE
errbase <- myData$`Actual Spending` - 260
errbasesq <- errbase^2
RMSEbase <- sqrt(mean(errbasesq))
