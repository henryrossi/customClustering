#' Implements custom clustering algorithm
#'
#' @param data A data frame
#' @param k An integer indicating the number of clusters
#' @param maxIterations An integer indicating the number of iterations
#'
#' @return A list containing the cluster assignments
#'
#' @export
cluster <- function(data, k, maxIteratoins = 100){
  # Check inputs
  if (!is.data.frame(data)){
    stop("Please enter your data as a data frame")
  }
  if (!is.numeric(k)){
    stop("Please enter a numeric value for k")
  }
  if (!is.numeric(maxIteratoins)){
    stop("Please enter a numeric value for maxIterations")
  }
  if ("character" %in% sapply(data, class)){
    stop("Please only enter numeric variables in your data frame")
  }

  data <- principleComponents(data)

  bestClusterMeasure <- 0
  a <- 1
  b <- 0.5

  for (x in 1:maxIteratoins){
    clusterOutput <- kClusters(data, k)

    # calculate cluster measure
    centroidDists <- Rfast::dista(clusterOutput$centroids, clusterOutput$centroids)
    seperation <- sum(centroidDists) / (2*nrow(centroidDists))
    compactness <- mean(rowSums((data - clusterOutput$centroids[clusterOutput$clusterAssignments, ]) ^ 2))
    clusterMeasure <- (a * seperation) / (b * compactness)

    if (!is.na(clusterMeasure) && !is.na(bestClusterMeasure && clusterMeasure > bestClusterMeasure)){
      bestClusterMeasure <- clusterMeasure
      bestClustering <- clusterOutput
    }
  }

  return(bestClustering)
}

#' Converts data of many dimensions into 2-dimensional data containing the 2 most principle components
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
#' @return A list containing the clustering assignments and the centroids
#'
kClusters <- function(data, k) {
  # Randomly initialize centroids
  centroids <- data[sample(1:nrow(data), k), ]

  converged <- FALSE
  while (!converged) {
    # Assign each data point to the nearest centroid
    distances <- Rfast::dista(data, centroids)
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
