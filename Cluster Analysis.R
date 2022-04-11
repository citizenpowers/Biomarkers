#Cluster analysis

library(cluster)
library(NbClust)
KM <-  as.data.frame(Soils_Wide_Scale_Tidy)


rownames(KM) <- KM$STATION 

KM <- KM[,3:23]

k2 <- kmeans(KM, centers = 2, nstart = 25)
k3 <- kmeans(KM, centers = 3, nstart = 25)
k4 <- kmeans(KM, centers = 4, nstart = 25)
k5 <- kmeans(KM, centers = 5, nstart = 25)
k6 <- kmeans(KM, centers = 6, nstart = 25)


fviz_cluster(k2, data = KM)
fviz_cluster(k3, data = KM,)
fviz_cluster(k4, data = KM)
fviz_cluster(k5, data = KM)
fviz_cluster(k6, data = KM)

gap_stat <- clusGap(KM, FUN = kmeans, nstart = 25,K.max = 10, B = 50)

fviz_gap_stat(gap_stat)
