####################################################
## Similarities and distances utilities
####################################################


#' Compute a matrix with the cosine similarity [-1, 1] between each column in the input matrix.
#' 
#' @param x The matrix or data.frame from which calculates cosine similarities
#' @return An adjency matrix with the cosine similarities between each column pair
cosineSim <- function(x) {
  x <- as.matrix(x)
  y <- t(x) %*% x
  res <- y / (sqrt(diag(y)) %*% t(sqrt(diag(y))))
  return(res)
}


#' Compute a matrix with the cosine distance [0, 2] between each column in the input matrix.
#' 'dist' does not support cosine method by default and that it's a custom alternative
#' to proxy package.
#' Equivalent to 'proxy::dist(as.matrix(x), method = "cosine")'
#' Source (pvclust examples): https://cran.r-project.org/web/packages/pvclust/pvclust.pdf
#' 
#' @param x The matrix or data.frame from which calculates cosine distances
#' @return A distance matrix (object of class 'dist') with the cosine distances between each column pairs
cosineDist <- function(x) {
  x <- as.matrix(x)
  y <- t(x) %*% x
  res <- 1 - y / (sqrt(diag(y)) %*% t(sqrt(diag(y))))
  res <- as.dist(res)
  attr(res, "method") <- "cosine"
  return(res)
}