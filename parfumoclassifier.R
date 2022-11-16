library(tidyverse)
library(cluster)
library(Rtsne)
library(ggplot2)
library(ggrepel)
library(readxl)
library(readxl)

set.seed(1494)

#Import
parfumomaster <- read_excel("~/R/Projects/ParfumoCluster/parfumomaster.xlsx")
View(parfumomaster)

#Copy data.

parfumo <- parfumomaster

#Create subset copy with only fragrance, sex, and accords. 
#Other variables will be used for visualization later.
#rating is unimportant in this analysis because the data was scraped from highest weighted rating. All perfumes in this list have a very high score
parfumosubset <- data.frame(parfumo)
parfumosubset <- parfumosubset[c(-2,-3,-5,-6,-7)]
view(parfumosubset)


#Change categorical to factors.


catcolumns <- c(2:22)
parfumosubset[catcolumns] <- lapply(parfumosubset[catcolumns], factor)

#Gower dissimilarity matrix.

gower_dist <- daisy(parfumosubset[2:22],
                    metric = "gower",
                    type = list()
                    )
gower_matrix <- as.matrix(gower_dist)

#Silhoutte to find correct k for PAM.

sil_width <- c(NA)
for(i in 2:10){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

#Choose 9 clusters, highest value.

#Partitioning Around Mediods (PAM) was chosen as the clustering algorithm. K-Means does not work here because the data is categorical. 

pam_fit <- pam(gower_dist, diss = TRUE, k = 10)
pam_results <- parfumosubset %>%
  dplyr::select(-fragrance) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary
view(parfumosubset[pam_fit$medoids, ])

#Rtsne package is used here to create a new data frame with coordinates, cluster, and fragrance name. 

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE, perplexity = 20)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(Cluster = factor(pam_fit$clustering),
         Fragrance = parfumosubset$fragrance)
tsne_data

#Plot clusters

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = Cluster)) 
  
