
#Libraries, Install

library(tidyverse)
library(cluster)
library(Rtsne)
library(ggplot2)
library(ggrepel)
library(readxl)

#Read excel file and create data frame
#Data frame will have 20 observations and 13 columns: Fragrance Name, Sex, and 11 columns for a variety of accords

TestFragSheet <- read_excel("~/R/TestFragSheet.xlsx")
fragdf <- data.frame(TestFragSheet)

#Convert sex column into factors where Male = 1, Female = 2, Unisex = 3

TestFragSheet$Sex <- as.factor(TestFragSheet$Sex)

#Gower metric to calculate dissimilarity with mixed data types 

gower_dist <- daisy(fragdf[2:13],
                    metric = "gower",
                    type = list()
)

#Create distance matrix and return least dissimilar and most dissimilar pairs

gower_mat <- as.matrix(gower_dist)
fragdf[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
fragdf[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

#Calculate silhouette width for PAM k
sil_width <- c(NA)
for(i in 2:10){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}
#Plot sihouette
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

#Silhouette indicates 9 clusters is ideal for this data set. However, the sample size at this time is likely not high enough to support this
#6 Clusters were chosen as increasing to 7 or 8 sees a drop off in the silhouette before increasing again

#Partitioning Around Mediods (PAM) was chosen as the clustering algorithm. K-Means is not optimal here because the data is binary/factor

pam_fit <- pam(gower_dist, diss = TRUE, k = 6)
pam_results <- fragdf %>%
  dplyr::select(-Fragrance) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

fragdf[pam_fit$medoids, ]

#Rtsne package is used here to create a new data frame with coordinates, cluster, and fragrance name 
#Perplexity had to be adjusted downward due to the small size of the matrices. When more data is added, we can use (N)umber of cells ^ (1/2)


tsne_obj <- Rtsne(gower_dist, is_distance = TRUE, perplexity = 6)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(Cluster = factor(pam_fit$clustering),
         Fragrance = fragdf$Fragrance)
tsne_data

#Plotting the clusters
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = Cluster)) +
  geom_text_repel(aes(label = fragdf$Fragrance))

