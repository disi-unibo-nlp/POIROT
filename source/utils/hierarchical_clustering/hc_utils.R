########################################################################################################
##
## Hierarchical Cluster Analysis (HCA)
##
## HCA allows the discovery of relationships between data.
## HCA works as follows:
## 1) puts each data point in its own cluster;
## 2) identifies the closest two clusters and combine them into one cluster;
## 3) repeats the above step till all the data points are in a single cluster.
## There are a few ways (supported also by hclust) to determine how close two clusters are:
## - complete linkage (default)
##   cluster distance = maximum distance between their individual components
## - single linkage
##   cluster distance = minimum distance between their individual components
## - mean linkage
##   cluster distance = average distance between their individual components
## - centroid linkage
##   cluster distance = distance between their centroids
##
## This file provides utils for two HCA methods
##
## METHOD 1) HCLUST - STANDARD HC
## -------------------------------
## - 'hclust' requires us to provide the data in the form of a distance matrix
## - 'hclust' by default clusters rows
##
## METHOD 2) PVCLUST - HC WITH P-VALUES
## -------------------------------------
## - Repository: https://github.com/shimo-lab/pvclust
## - Paper: https://academic.oup.com/bioinformatics/article/22/12/1540/207339
## - Source: https://www.statmethods.net/advstats/cluster.html
## - Evaluates the uncertainty in HCA. For each cluster in HC, p-values are calculated via multiscale
##   bootstrap resampling. P-value of a cluster is a quantitative value between 0 and 1, which
##   indicates how strong the cluster is supported by data
## - AU is a type of p-value provided by 'pvclust'
## - Higher is the number of bootstrap replications, more confident is the estimation accuracy of
##   AU p-values as well as the existance of highlighted clusters
## - 'pvclust' performs HCA via function 'hclust' (so it internally calls function 'dist') and
##   automatically computes p-values for all clusters contained in the clustering of original data
## - 'pvclust' expects a matrix or a dataframe (not distance matrix)
## - 'pvclust' clusters columns (not rows)
##
########################################################################################################


#' Perform a standard HCA through hclust, with also support to cosine metric distance.
#' Displays resulting dendrogram and calculates clusters at a certain height.
#' 
#' @param x The matrix or data.frame with the rows to cluster
#' @param dist_method The distance method to use for matrix rows (cosine also supported)
#' @param hclust_method The clustering method
#' @param cut_height The heights where the dendrogram should be cut
#' @param labels The labels of the items
#' @return The agglomerate object, the dataframe with data and related cluster index,
#' and the aggregated list with an item list for each cluster.
standardHca <- function(x, dist_method = "cosine",
                        hclust_method = "ward.D2", cut_height = 0.05,
                        labels) {
  
  if (dist_method == "cosine") {
    # Compute a cosine distance matrix
    dist_matrix <- cosineDist(t(x))
    dist_matrix[is.na(dist_matrix)] <- 0
  } else {
    # Compute a distance matrix using a method supported by 'dist'
    dist_matrix = dist(x, method = dist_method)
  }
  
  # Perform a hiearchical cluster analysis on rows' items (based on cosine similarity)
  # and plots results as an edited dendrogram
  # Source: https://bit.ly/2QO0OZ6
  h_agglomerate <- hclust(dist_matrix, method = hclust_method)
  
  # Cut dendrogram tree into some clusters based on height
  # (in fact the number of clusters is unknown and depends by the analysis)
  # and order items by them cluster
  # Gets a dataframe with items as rows' names and their cluster index as column
  # Note: hclust does not give cluster groups
  clusters <- stats::cutree(h_agglomerate, h = cut_height)[h_agglomerate$order]
  
  # Create a dataframe with reorganized clusters data
  clusters <- data.frame(data = labels, cluster = clusters)
  
  # Aggregate items in list by cluster identifier
  aggregated_clusters <- aggregate(as.character(data) ~ cluster,
                                   clusters,
                                   c)
  
  # Convert clusters to list of list
  aggregated_clusters <- split(aggregated_clusters[, 2], seq(nrow(aggregated_clusters)))
  
  list(
    agglomerate = h_agglomerate,
    clusters = clusters,
    aggregated_clusters = aggregated_clusters
  )
  
}


#' Perform a HCA based on multiscale bootstrap resampling through pvclust, with cosine metric distance.
#' Displays resulting dendrogram and calculates clusters highly supported by data (high pvalue).
#' 
#' @param x The matrix or data.frame with the rows to cluster
#' @param dist_method The distance method to use for matrix rows (cosine also supported)
#' @param hclust_method The clustering method
#' @param nboot The number of bootstrap replications
#' @param r The scale parameter for samples definition
#' @param parallel Boolean. True for parallel computation
#' @param min_pvalue The minimum AU p-value for clusters retrieving
#' @param terms The labels to display in the dendrogram
#' @return The agglomerate object and the list with an item list for each cluster.
pvalueHca <- function(x, dist_method = "cosine",
                      hclust_method = "ward.D2", nboot = 1000,
                      r = seq(.5, 1.4, by = .1), parallel = TRUE,
                      min_pvalue = .95, labels) {
  
  # Perform a hierarchical clustering based on multiscale bootstrap resampling
  # Clusters that are highly supported by the data will have large p-values
  # Custom scale parameter (r) according to:
  # https://stackoverflow.com/questions/12897925/error-with-multiscale-hierarchical-clustering-in-r
  h_agglomerate <- pvclust::pvclust(t(x),
                                    method.dist = if (dist_method == "cosine") cosineDist else dist_method,
                                    method.hclust = hclust_method,
                                    nboot = nboot,
                                    r = r,
                                    parallel = parallel)
  
  # Retrieve clusters data
  h_agglomerate$hclust$labels <- labels
  clusters <- pvclust::pvpick(h_agglomerate, alpha = min_pvalue, pv = "au", type = "geq")
  
  list(
    agglomerate = h_agglomerate,
    clusters = clusters
  )
  
}