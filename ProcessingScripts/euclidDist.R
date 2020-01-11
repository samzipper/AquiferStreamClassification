## euclidDist
#' Function to find euclidean distance from a set of points to a cluster centers.
#' 
#' Input is:
#'   -points1 = matrix of points
#'   -points2 = matrix of cluster centers
#'
#' Output is:
#'   -distanceMatrix = matrix of distances from point to cluster center. 
#'                     Rows correspond to each point, columns to distance to each cluster center
#' 
#' From: https://stackoverflow.com/questions/27082378/how-to-compute-distances-between-centroids-and-data-matrix-for-kmeans-algorithm

euclidDist <- function(points1, points2){
  distanceMatrix <- matrix(NA, nrow=dim(points1)[1], ncol=dim(points2)[1])
  for(i in 1:nrow(points2)) {
    distanceMatrix[,i] <- sqrt(rowSums(t(t(points1)-points2[i,])^2))
  }
  distanceMatrix
}

# sample data:
#points1 <- matrix(rnorm(90), ncol=3)
#points2 <- points1[sample(nrow(points1), 4),]
#test.out <- euclidDist(points1, points2)
