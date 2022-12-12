#' Estimate log-likelihood
#'
#' Estimate the log-likelihood of each observation originating from the joint distribution of the features in \code{data} for the majority class
#'
#' @param data the complete set of features (both classes)
#' @param y outcome variable to be classified
#'
#' @return a vector containing the log-likelihood estimated


est_likelihood <- function(data,y){
  n <- nrow(data)
  p <- ncol(data)
  f_p <- matrix(0, nrow = n, ncol = p)
  mj_ind <- y==FALSE
  for(i in 1:p){
    if(is.numeric(data[,i])){
      d <- stats:: density(data[mj_ind,i])
      for(j in 1:n){
        f_p[j,i] <- d$y[which.min(abs(d$x-data[j,i]))]
      }
    } else {
      d <- table(data[mj_ind,i])/length(mj_ind)
      for(j in 1:n){
        f_p[j,i] <- as.numeric(d[data[j,i]])
      }
    }

  }
  log_like <- rowSums(log(f_p))
  return(log_like)
}
