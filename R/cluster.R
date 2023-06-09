#' Implements custom clustering algorithm
#'
#' @param data A data frame
#' @param k An integer indicating the number of clusters
#' @param pca A boolean value representing whether or not to perform PCA
#'        on the data before the clustering
#' @param iterations An integer indicating the number of iterations
#' @param a An integer indicating the weight separation (average distance
#'        between cluster centers) has when measuring the quality of the
#'        cluster
#' @param b An integer indicating the weight compactness (average within
#'        sum of squares) has when measuring the quality of the cluster
#'
#' @return A list containing the cluster assignments
#'
#' @importFrom Rfast dista
#'
#' @export
cluster <- function(data, k, pca = FALSE, iterations = 100, a = 1, b = 0.5){
  # Check inputs
  if (!is.data.frame(data)){
    stop("Please enter your data as a data frame")
  }
  if (!is.numeric(k)){
    stop("Please enter a numeric value for k")
  }
  if (!is.logical(pca)){
    stop("Please enter a logical value for the option pca")
  }
  if (!is.numeric(iterations)){
    stop("Please enter a numeric value for iterations")
  }
  if (!is.numeric(a) && !is.numeric(b)){
    stop("Please enter a numeric value for a and b")
  }
  if ("character" %in% sapply(data, class)){
    stop("Please only enter numeric variables in your data frame")
  }

  if (pca) {
    data <- principleComponents(data)
  }
  data <- as.matrix(data)

  bestClusterMeasure <- 0

  for (x in 1:iterations){
    clusterOutput <- kClusters(data, k)

    # calculate cluster measure
    centroidDists <- dista(clusterOutput$centroids, clusterOutput$centroids)
    separation <- sum(centroidDists) / (2*nrow(centroidDists))
    compactness <- mean(rowSums(
      (data - clusterOutput$centroids[clusterOutput$clusterAssignments, ]) ^ 2))
    clusterMeasure <- (a * separation) / (b * compactness)

    if (!is.na(clusterMeasure) && !is.na(bestClusterMeasure
                         && clusterMeasure > bestClusterMeasure)){
      bestClusterMeasure <- clusterMeasure
      bestClustering <- clusterOutput
    }
  }

  return(bestClustering)
}

#' Converts data of many dimensions into 2-dimensional data containing the
#' 2 most principle components
#'
#' @param data A data frame
#'
#' @return A matrix containing the 2 most principle component
#'
principleComponents <- function(data) {
  data <- princomp(data)
  data <- data$scores[, 1:2]
}

#' K-means clustering with random centroids
#'
#' @param data A matrix containing the 2 most principle components
#' @param k An integer indicating the number of clusters
#'
#' @importFrom Rfast dista
#'
#' @return A list containing the clustering assignments and the centroids
#'
kClusters <- function(data, k) {
  # Randomly initialize centroids
  centroids <- data[sample(1:nrow(data), k), ]

  converged <- FALSE
  while (!converged) {
    # Assign each data point to the nearest centroid
    distances <- dista(data, centroids)
    clusterAssignments <- apply(distances, 1, which.min)

    # Update centroids based on the mean of the assigned data points
    newCentroids <- t(sapply(1:k, function(i){
      cluster <- data[clusterAssignments == i, ]
      if (class(cluster)[1] == "numeric"){
        return(cluster)
      }
      colMeans(cluster)
    }))

    # Check convergence
    converged <- identical(centroids, newCentroids)

    centroids <- newCentroids
  }

  list(clusterAssignments = clusterAssignments, centroids = centroids)
}
