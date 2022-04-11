#Cluster analysis using method described https://uc-r.github.io/hc_clustering

library(cluster)
library(NbClust)
library(dendextend)

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
fviz_cluster(k5, data = KM)
fviz_cluster(k6, data = KM)

#find optimum number of clusters
gap_stat <- clusGap(KM, FUN = kmeans, nstart = 25,K.max = 10, B = 50)

#visualize 
fviz_gap_stat(gap_stat)


# Agglomerative and Divisive hierarchical clustering-----------------------------------

Soils_DF <-  as.data.frame(Soils_Wide_Scale_Tidy) %>%  #kmeans does not work well with Tibbles. Convert to DF
mutate(STATION=str_replace(STATION,"ST","")) #shorten station names for better display in figures
  
rownames(Soils_DF) <- Soils_DF$STATION    #Need rownames to identify stations

# Dissimilarity matrix
d <- dist(Soils_DF, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

hc2 <- agnes(Soils_DF, method = "complete")

# Agglomerative coefficient
hc2$ac

# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(df, method = x)$ac
}

hc3 <- agnes(Soils_DF, method = "ward")
pltree(hc3, cex = 1, hang = -1, main = "Dendrogram of AGNES") 

# compute divisive hierarchical clustering
hc4 <- diana(Soils_DF)

# Divise coefficient; amount of clustering structure found
hc4$dc

# plot dendrogram
pltree(hc4, cex = 1, hang = -1, main = "Dendrogram of DIANA")

# Ward's method
hc5 <- hclust(d, method = "ward.D2" )

# Cut tree into 4 groups
sub_grp <- cutree(hc5, k = 4)

# Number of members in each cluster
table(sub_grp)

plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 5, border = 2:5)

# Compute distance matrix
res.dist <- dist(Soils_DF, method = "euclidean")

# Compute 2 hierarchical clusterings
hc1 <- hclust(res.dist, method = "complete")
hc2 <- hclust(res.dist, method = "ward.D2")

# Create two dendrograms
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

tanglegram(dend1, dend2)

dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list), 2)),
           lab.cex =.8
           
)
