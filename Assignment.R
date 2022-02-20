#import the data file
wineData = read.csv("wine.csv", header = TRUE)

nrow(wineData)
ncol(wineData)
wineData[1]
wineData

#1.Remove Class feature
ncol(wineData)
wineData$Class<-NULL
wineData
ncol(wineData)

#2.Clean and prepare data set
summary(wineData)
plot(wineData)
which(is.na(wineData), arr.ind=TRUE)

max(wineData$Magnesium)
wineDataClean = wineData[wineData$Magnesium < 140,]
nrow(wineData)
nrow(wineDataClean)

max(wineData$Flavanoids)
wineDataClean1 = wineDataClean[wineDataClean$Flavanoids < 4,]
nrow(wineDataClean1)

max(wineData$Proline)
wineDataClean2 = wineDataClean1[wineDataClean1$Proline < 1600,]
nrow(wineDataClean2)

max(wineData$ColorIntensity)
wineDataClean3 = wineDataClean2[wineDataClean2$ColorIntensity < 11,]
nrow(wineDataClean3)

#3.Use feature engineering technique
var(wineDataClean3$Alcohol)
sd(wineDataClean3$Alcohol)
IQR(wineDataClean3$Alcohol)

var(wineDataClean3$MalicAcid)
sd(wineDataClean3$MalicAcid)
IQR(wineDataClean3$MalicAcid)

var(wineDataClean3$TotalPhenols)
sd(wineDataClean3$TotalPhenols)
IQR(wineDataClean3$TotalPhenols)

par(mfrow=c(1, 3))
hist(wineDataClean3$Alcohol, cex.main = 0.75)
hist(wineDataClean3$MalicAcid, cex.main = 0.75)
hist(wineDataClean3$TotalPhenols, cex.main = 0.75)

par(mfrow=c(1, 3))
p = ecdf(wineDataClean3$Alcohol)
plot(p, cex.main = 0.75)
p = ecdf(wineDataClean3$MalicAcid)
plot(p, cex.main = 0.75)
p = ecdf(wineDataClean3$TotalPhenols)
plot(p, cex.main = 0.75)

par(mfrow=c(1, 3))
boxplot(wineDataClean3$Alcohol)
boxplot(wineDataClean3$MalicAcid)
boxplot(wineDataClean3$TotalPhenols)

d = density(wineDataClean3$Alcohol)
d
par(mfrow=c(1, 2))
plot(wineDataClean3$Alcohol, cex.main = 0.75)
plot(d, cex.main = 0.75)

d = density(wineDataClean3$MalicAcid)
d
par(mfrow=c(1, 2))
plot(wineDataClean3$MalicAcid, cex.main = 0.75)
plot(d, cex.main = 0.75)

d = density(wineDataClean3$TotalPhenols)
d
par(mfrow=c(1, 2))
plot(wineDataClean3$TotalPhenols, cex.main = 0.75)
plot(d, cex.main = 0.75)

par(mfrow=c(1, 2))
boxplot(TotalPhenols~Alcohol, data = wineDataClean3)
boxplot(MalicAcid~Alcohol, data = wineDataClean3)

library(e1071)
skewness(wineData$Alcohol)
kurtosis(wineData$Alcohol)

skewness(wineData$MalicAcid)
kurtosis(wineData$MalicAcid)

skewness(wineData$TotalPhenols)
kurtosis(wineData$TotalPhenols)


#Normalize Data with Standard Scaling in R
wineDataNorm <- as.data.frame(scale(wineDataClean3))
wineDataNorm

#Normalize Data with Min-Max Scaling in R
library(caret)
wineDataProcess <- preProcess(as.data.frame(wineDataClean3), method=c("range"))
wineDataNormScale <- predict(wineDataProcess, as.data.frame(wineDataClean3))
wineDataNormScale

#4.Apply clustering algorithms
plot(wineDataNormScale)
plot(wineDataNormScale[,1:3])

#k-means clustering
kcWineData = kmeans(wineDataNormScale[,1:3], 2)
kcWineData
par(mfrow=c(1, 2))
plot(wineDataNormScale[,1:2], col=kcWineData$cluster)
points(kcWineData$centers[,1:2], col=1:2, pch=8, cex=2)
plot(wineDataNormScale[,2:3], col=kcWineData$cluster)
points(kcWineData$centers[,2:3], col=1:2, pch=8, cex=2)


#Hierarchical clustering
dis = dist(wineDataNormScale[1:3], method="euclidean")
hcWineDataAve = hclust(dis, method="ave")
hcWineDataWard = hclust(dis, method="ward.D")
par(mfcol=c(1,2))
plot(hcWineDataAve, hang=-1, cex.main = 0.75, cex.axis = 0.5)
rect.hclust(hcWineDataAve, k=2, border="green")
plot(hcWineDataWard, hang=-1, cex.main = 0.75, cex.axis = 0.5)
rect.hclust(hcWineDataWard, k=2, border="green")
hcWineDataAveCut = cutree(hcWineDataAve, 2)
hcWineDataAveCut
hcBrainWardCut = cutree(hcWineDataWard, 2)
par(mfcol=c(1,2))
plot(wineDataNormScale[,1:2], col=hcWineDataAveCut, cex.main = 0.75)
plot(wineDataNormScale[,1:2], col=hcBrainWardCut, cex.main = 0.75)
plot(wineDataNormScale[,2:3], col=hcWineDataAveCut, cex.main = 0.75)
plot(wineDataNormScale[,2:3], col=hcBrainWardCut, cex.main = 0.75)


#5. Evaluate clustering(objective measures to evaluate clustering)
install.packages("factoextra")
library("factoextra")
distance <- get_dist(wineDataNormScale)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#elbow method
set.seed(123)
fviz_nbclust(wineDataNormScale, kmeans, method = "wss")

#Average Silhouette Method
fviz_nbclust(wineDataNormScale, kmeans, method = "silhouette")

set.seed(123)
finalWineData <- kmeans(wineDataNormScale, 3, nstart = 25)
print(finalWineData)
fviz_cluster(finalWineData, data = wineDataNormScale)


#6.Correlation
cor(wineDataNormScale$Alcohol, wineDataNormScale$MalicAcid)
cor(wineDataNormScale$Alcohol, wineDataNormScale$TotalPhenols)
cor(wineDataNormScale$MalicAcid, wineDataNormScale$TotalPhenols)
cor(wineData)
