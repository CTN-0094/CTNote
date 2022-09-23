# Cluster Outcomes 
# Gabriel Odom
# 2022-03-10
# UPDATED: 2022-08-18


library(ctn0094data)
library(ctn0094DataExtra)
library(CTNote)
library(tidyverse)

# We use ctn0094data v. 0.0.0.9023 and ctn0094DataExtra v. 0.0.0.9009
# The original draft of this code is in: 
#   Box/CTN0094/ctn0094_data_gabriel/inst/scripts/cluster_outcomes_20220310.R
# and
# Box/CTN0094/ctn0094_data_gabriel/inst/scripts/cluster_outcomes_bootstrap_20220310.R



######  IMPORT AND JOIN  ######################################################
# This section in its detail is no longer necessary
data("outcomesCTN0094")



######  Transform  ############################################################

is_count <- function(x, ignore_na = TRUE) {
  if (!is.numeric(x)) {
    return(FALSE)
  } else {
    max(x, na.rm = ignore_na) > 1 
  }
}

outcomesScaled_df <- 
  outcomesCTN0094 %>% 
  select(-who, -usePatternUDS) %>% 
  # Scale count features
  mutate(
    across(.cols = where(is_count), ~ { .x / max(.x) } )
  ) %>% 
  # Sort outcomes from same authors together
  select(sort(colnames(.))) 



rm(outcomesCTN0094, is_count)



######  Bootstrap Clustering  #################################################


###  Functions  ###
ClusterToGraph <- function(x_clust, k_int) {
  # I have a 65 x 1 vector of cluster membership assignments ("1", "2", "3",
  #   etc). We need to turn this into a 65 x 65 matrix of neighbours: 1 if they
  #   are in the same cluster, 0 otherwise.
  # browser()
  
  clusters_int <- cutree(x_clust, k = k_int)
  features_char <- x_clust[["labels"]]
  
  sapply(
    features_char,
    FUN = function(name_char){
      
      targetCluster <- clusters_int[name_char]
      clusters_int == targetCluster
      
    }
  )
  
}

BootClusterGraph <- function(x_df, clusterSizes_int) {
  
  ###  Resample Phi and Cluster  ###
  keepRows_int <- sample(nrow(x_df), size = nrow(x_df), replace = TRUE)
  xPhi_mat <- cor(x_df[keepRows_int, ], use = "pairwise.complete.obs")
  x_dist <- as.dist(1 - abs(xPhi_mat))
  x_hclust <- hclust(x_dist)
  
  
  ###  Return Map Over K  ###
  clusterGraph_ls <- map(
    .x = clusterSizes_int,
    .f = ~{
      ClusterToGraph(x_clust = x_hclust, k_int = .x)
    }
  )
  names(clusterGraph_ls) <- paste0("k_", clusterSizes_int)
  
  clusterGraph_ls
  
}


###  Apply  ###
set.seed(1234)
system.time(
  bootstrappedClusters_ls <- replicate(
    n = 10000,
    { BootClusterGraph(outcomesScaled_df, clusterSizes_int = 3) },
    simplify = FALSE
  )
)
# k = 3: 69.7 sec for 1k; 11.65 min for 10k
# NOTE 2022-08-18: in the Box/CTN0094/ctn0094_data_gabriel/ version, we used
#   cluster sizes 2:15. We noted an elbow in the scree plot at k = 3, and Laura
#   believed there to be 3 clusters a priori (but she thought they would be
#   abstinence, relapse, and reduction).


###  Collapse  ###
wrangledBootClusters_mat <- 
  bootstrappedClusters_ls %>% 
  map(`[[`, 1) %>% 
  reduce(`+`) %>%
  `/`(10000) 

# saveRDS(wrangledBootClusters_mat, file = "vignettes/outcome_clusters.rda")


######  Correlation and Cluster Plots  ########################################

library(corrplot)


###  Clustering  ###
corrplot.mixed(wrangledBootClusters_mat, order = 'hclust', tl.pos = "n")
outcomes_dist <- as.dist(1 - wrangledBootClusters_mat)
outcomes_hclust <- hclust(outcomes_dist)
plot(outcomes_hclust)

# Add cutoff to plot
p <- ncol(outcomesScaled_df)
cutClust_int <- p - 3 # number of clusters
cutHeight_num <- mean(
  outcomes_hclust$height[cutClust_int:(cutClust_int + 1)]
)
abline(h = cutHeight_num, lty = 2)


###  More Dendrogram Options  ###
# Code from: 
# http://www.sthda.com/english/wiki/beautiful-dendrogram-visualizations-in-r-5-must-known-methods-unsupervised-machine-learning
library(ape)

colors_char <- c("#db6d00", "#490092", "#009292")
outcomes3Clust_int <- cutree(outcomes_hclust, 3)
plot(
  as.phylo(outcomes_hclust),
  cex = 0.7,
  tip.color = colors_char[outcomes3Clust_int],
  no.margin = TRUE
)

