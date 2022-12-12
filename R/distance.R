#' Euclidean distance
#'
#' Calculate the euclidean distance of the features in \code{data}
#' of each observation from the center of the majority class
#' @param data the complete set of features (both classes)
#' @param y outcome variable to be classified
#'
#' @return a vector containing the distances

distance <- function(data,y){
  n <- nrow(data)
  p <- ncol(data)
  d <- matrix(0, nrow = n, ncol = p)
  mj_ind <- y==FALSE
  for(i in 1:p){
    if(is.numeric(data[,i])){
      v <- scale(data[,i])
      m_j <- mean(v[mj_ind])
      d[,i] <- (v-m_j)^2
    } else {
      freq <- table(data[,i])/n
      d[,i] <- (1-as.numeric(freq[data[,i]]))^2
    }
  }
  dist <- rowSums(d)
  return(dist)
}
