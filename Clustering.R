#agglomerative clustering
library(readxl)

#install.packages("cluster")
library(cluster)

myData <- read_excel("C:/Users/willa/Downloads/Ch14_Q9_V19_Data_File.xlsx")
View(myData)
myData_std <- scale(myData[,2:5]) #standardize the numerical variables - note this data frame now excludes School and City
View(myData_std)
d <- dist(myData_std, method = "euclidean") #euclidean for numerical records; if distance is small, they're similar. 
#method = binary (jaccard's) for class variables, Gower for mixed variables.
aResult <- agnes(d, diss=TRUE, method="ward") #agnes function for agglomerative (hierarchical) clustering, ward for ward's method
aResult
plot(aResult) #with min dist of 15, 2 clusters.


#part b


cityData <- subset(myData, myData$City ==1)
View(cityData)
#cityData <- myData[which(myData$City==1)]

#cityObs <- split(myData, myData$City, drop = FALSE)
#View(cityObs)

myData_std <- scale(cityData[,2:5]) #standardize the numerical variables - note this data frame now excludes School and City
View(myData_std)
d <- dist(myData_std, method = "euclidean") #euclidean for numerical records; if distance is small, they're similar. 
#method = binary (jaccard's) for class variables, Gower for mixed variables.
aResult <- agnes(d, diss=TRUE, method="ward") #agnes function for agglomerative (hierarchical) clustering, ward for ward's method
aResult
plot(aResult) #with min dist of 10, city only, 2 clusters.


#part c


cityData <- subset(myData, myData$City ==0)
View(cityData)
#cityData <- myData[which(myData$City==1)]

#cityObs <- split(myData, myData$City, drop = FALSE)
#View(cityObs)

myData_std <- scale(cityData[,2:5]) #standardize the numerical variables - note this data frame now excludes School and City
View(myData_std)
d <- dist(myData_std, method = "euclidean") #euclidean for numerical records; if distance is small, they're similar. 
#method = binary (jaccard's) for class variables, Gower for mixed variables.
aResult <- agnes(d, diss=TRUE, method="ward") #agnes function for agglomerative (hierarchical) clustering, ward for ward's method
aResult
plot(aResult) #with min dist of 10, city only, 2 clusters.


#part d


aClusters <- cutree(aResult, k=2)
plot(aClusters)

myData <- data.frame(myData, aClusters)
View(myData)

summary(subset(myData, aClusters == 1))
summary(subset(myData, aClusters == 2))
#summary(subset(myData, aClusters == 3))

summary(as.factor(aClusters)) #need catergorical values to count the number of observations in each cluster

