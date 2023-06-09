#' Implements custom clustering algorithm
#'
#' @param data A data frame
#' @param k An integer indicating the number of clusters
#'
#' @return A list containing the cluster assignments
#'
#' @export
cluster <- function(data, k){
  # Check inputs
  if (!is.data.frame(data)){
    stop("Please enter your data as a data frame")
  }
  if (!is.numeric(k)){
    stop("Please enter a numeric value for k")
  }
  if ("character" %in% sapply(data,class)){
    stop("Please only enter numeric variables in your data frame")
  }
  data <- principleComponents(data)
}

#' Converts data of many dimensions into 2-dimensional data containing the 2 most principle components
#'
#' @param data A data frame
#'
#' @return A data frame containing the 2 most principle component
#'
principleComponents <- function(data) {
  data <- princomp(data)
  data <- as.data.frame(data$scores)
  data <- data %>% select(c(Comp.1, Comp.2))
}
