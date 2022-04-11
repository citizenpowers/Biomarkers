#Dimension reduction 


library(ggforce)
library(tidymodels)
library(bestNormalize)

library(factoextra)


# PCA ----------------------------------------------------------


res.pca <- prcomp(select(ungroup(Soils_Wide_Tidy),3:24),scale = TRUE)

fviz_eig(res.pca)

fviz_pca_ind(res.pca,col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)

fviz_pca_biplot(res.pca, repel = TRUE, col.var = "#2E9FDF", col.ind = "#696969" )



# Sorenson Multipart dissimilarity Index ----------------------------------


             