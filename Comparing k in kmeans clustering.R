#hw12p3
suppressWarnings(RNGversion("3.5.3"))

library(readxl)

library(cluster)

myData <- read_excel("C:/Users/willa/Downloads/Ch14_Q31_Data_File.xlsx")
View(myData)
myData <- scale(myData)
View(myData)

# 2. perform 4-means clustering 
set.seed(1)
kResult <- pam(myData, k=4)
summary(kResult)

#generate plots
plot(kResult)


#part b


myData <- read_excel("C:/Users/willa/Downloads/Ch14_Q31_Data_File.xlsx")
#View(myData)
#myData <- scale(myData)
#View(myData)

# 2. perform 3-means clustering 
set.seed(1)
kResult <- pam(myData, k=3)
summary(kResult)

#generate plots
plot(kResult)


#part c

myData <- read_excel("C:/Users/willa/Downloads/Ch14_Q31_Data_File.xlsx")
#View(myData)
#myData <- scale(myData)
#View(myData)

# 2. perform 3-means clustering 
set.seed(1)
kResult <- pam(myData, k=4)
summary(kResult)

#generate plots
plot(kResult)


