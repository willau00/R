#hw12p2

library(readxl)

library(cluster)

myData <- read_excel("C:/Users/willa/Downloads/Ch14_Q22_R_V10_Data_File.xlsx")
View(myData)
d <- daisy(myData[,1:8], metric = "gower") #euclidean for numerical records; if distance is small, they're similar. 
#method = binary (jaccard's) for class variables, Gower for mixed variables.
d
mResult <- agnes(d, method="ward") #agnes function for agglomerative (hierarchical) clustering, ward for ward's method
mResult
plot(mResult) #with min dist of 15, 2 clusters.

aClusters <- cutree(mResult, k=4)
plot(aClusters)

summary(as.factor(aClusters))
