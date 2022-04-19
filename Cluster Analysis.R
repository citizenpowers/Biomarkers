#Cluster analysis using method described https://uc-r.github.io/hc_clustering

library(cluster)
library(NbClust)
library(dendextend)
library(readr)
library(readxl)
library(scales)
library(dplyr)
library(ggpmisc)
library(ggplot2)
library(lubridate)
library(tidyr)
library(stringr)
library(ggrepel)
library(factoextra)

# Import Data -------------------------------------------------------------


Soils_Wide_Scale_Tidy <- read_csv("Data/Soils_Wide_Scale_Tidy.csv")


# K-means analysis --------------------------------------------------------

KM <-  as.data.frame(Soils_Wide_Scale_Tidy)  #kmeans does not work well with Tibbles. Convert to DF

rownames(KM) <- KM$STATION    #Need rownames to identify stations

KM <- KM[,3:23]  #keep only numeric data. rownames will identify stations

k2 <- kmeans(KM, centers = 2, nstart = 25)  #Run kmeans for 2 clusters
k3 <- kmeans(KM, centers = 3, nstart = 25)  #Run kmeans for 3 clusters
k4 <- kmeans(KM, centers = 4, nstart = 25)  #Run kmeans for 4 clusters
k5 <- kmeans(KM, centers = 5, nstart = 25)  #Run kmeans for 5 clusters
k6 <- kmeans(KM, centers = 6, nstart = 25)  #Run kmeans for 6 clusters

#Visualize cluster analysis
fviz_cluster(k2, data = KM)
fviz_cluster(k3, data = KM)
fviz_cluster(k4, data = KM)
K5_fig <- fviz_cluster(k5, data = KM,ggtheme = theme_bw(),main = "K-Means Cluster Plot: 5 Groups")
fviz_cluster(k6, data = KM)

#Save 
ggsave("./Figures/K-Means Cluster Plot- 5 Groups.jpeg",plot=K5_fig ,height=8,width=10,units="in")


#find optimum number of clusters
gap_stat <- clusGap(KM, FUN = kmeans, nstart = 25,K.max = 10, B = 50)

#visualize 
fviz_gap_stat(gap_stat)


# Agglomerative and Divisive hierarchical clustering-----------------------------------

Soils_DF <-  as.data.frame(Soils_Wide_Scale_Tidy) %>%  #kmeans does not work well with Tibbles. Convert to DF
#mutate(STATION=str_replace(STATION,"ST","")) %>% #shorten station names for better display in figures
select(-STA)  #remove STA column  

rownames(Soils_DF) <- Soils_DF$STATION    #Need rownames to identify stations

# Dissimilarity matrix
d <- dist(Soils_DF, method = "euclidean")

# Hierarchical clustering using all methods Linkage
hc1 <- hclust(d, method = "complete")
hc2 <- hclust(d, method = "single" )
hc3 <- hclust(d, method = "average" )
hc4 <- hclust(d, method = "ward.D2" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)
plot(hc2, cex = 0.6, hang = -1)
plot(hc3, cex = 0.6, hang = -1)
plot(hc4, cex = 0.6, hang = -1)


# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(df, method = x)$ac
}

# Agglomerative coefficient
ac1 <- agnes(Soils_DF, method = "complete")
ac2 <- agnes(Soils_DF, method = "single")
ac3 <- agnes(Soils_DF, method = "average")
ac4 <- agnes(Soils_DF, method = "ward")

ac1$ac
ac2$ac
ac3$ac
ac4$ac

pltree(ac1, cex = 1, hang = -1, main = "Dendrogram of Agglomerative Complete") 
pltree(ac2, cex = 1, hang = -1, main = "Dendrogram of Agglomerative Single") 
pltree(ac3, cex = 1, hang = -1, main = "Dendrogram of Agglomerative Average") 
Ward_fig_Agg <- pltree(hc4, cex = 1, hang = -1, main = "Dendrogram of Agglomerative Ward") 

png(filename="./Figures/Dendrogram of Agglomerative Ward.png",width = 12, height = 6,units = "in",res   = 600,pointsize = 4)
plot(hc4, cex = 2, hang = -1)
rect.hclust(hc4, k = 5, border = 2:5)
dev.off()

# compute divisive hierarchical clustering
dc1 <- diana(Soils_DF)
rect.hclust(dc1, k = 5, border = 2:5)

# Divisive coefficient; amount of clustering structure found
dc1$dc

# plot dendrogram
pltree(dc1, cex = 1, hang = -1, main = "Dendrogram of Soil Sites")

png(filename="./Figures/Dendrogram of Divisive Hierarchical Clustering.png",width = 12, height = 6,units = "in",res   = 600,pointsize = 4)
pltree(dc1, cex = 2, hang = -1, main = "Dendrogram of Divisive Hierarchical Clustering") 
rect.hclust(dc1, k = 5, border = 2:5)
dev.off()

# Ward's method
hc5 <- hclust(d, method = "ward.D2" )

# Cut tree into 4 groups
sub_grp <- cutree(hc5, k = 4)

# Number of members in each cluster
table(sub_grp)

plot(hc5, cex = 1)
rect.hclust(hc5, k = 5, border = 2:5)

# Compute distance matrix
res.dist <- dist(Soils_DF, method = "euclidean")

# Compute 2 hierarchical clusterings
hc1 <- hclust(res.dist, method = "complete")
hc2 <- hclust(res.dist, method = "ward.D2")

# Create two dendrograms
dend1 <- as.dendrogram (hc4)
dend2 <- as.dendrogram (hc3)

tanglegram(dend1, dend2)

dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = TRUE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list), 2)),
           lab.cex =.8)
