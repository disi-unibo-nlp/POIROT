##########################################
## Algebra Utils
##########################################


#' Converts a number range to another range, maintaining ratio.
#' 
#' @param x The value in the old range to convert.
#' @param from_min The minimum value of the old range.
#' @param from_max The maximum value of the old range.
#' @param to_min The minimum value of the new range.
#' @param to_max The maximum value of the new range.
#' @return The value in the new range.
changeRange <- function(x, from_min, from_max, to_min, to_max) {
  
   # Figures out how wide each range is
  from_span <- from_max - from_min
  to_span <- to_max - to_min
  
  # Converts the left range into a [0, 1] range
  x_scaled = (x - from_min) / from_span
  
  # Converts the value from the [0, 1] range to the specified range
  new_x <- to_min + (x_scaled * to_span)
  
  return(new_x)
  
}

# Gets lower triangle of a matrix
getLowerTri <- function(mat){
  mat[upper.tri(mat)] <- NA
  return(mat)
}

# Gets upper triangle of a matrix
getUpperTri <- function(mat){
  mat[lower.tri(mat)]<- NA
  return(mat)
}

# Applies softmax function (https://rpubs.com/FJRubio/softmax)
softmax <- function(par){
  n.par <- length(par)
  par1 <- sort(par, decreasing = TRUE)
  Lk <- par1[1]
  for (k in 1:(n.par-1)) {
    Lk <- max(par1[k+1], Lk) + log1p(exp(-abs(par1[k+1] - Lk))) 
  }
  val <- exp(par - Lk)
  return(val)
}