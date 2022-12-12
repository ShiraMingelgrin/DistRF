#' Accuracy measures
#'
#' @param tab Confusion matrix with prediction as columns
#'
#' @return a list containing the following entries
#' \itemize{
#'    \item{True positive rate}
#'    \item{True negative rate}
#'    \item{Area under the curve}
#'    \item{ggplot object for the ROC curve}
#'  }
acc_meas <- function(tab){
  tpr <- tab[2,2]/sum(tab[2,1:2])
  tnr <- tab[1,1]/sum(tab[1,1:2])
  auc <- (tpr*(1-tnr)/2)+(tpr*tnr)+(tnr*(1-tpr)/2)
  roc <- ggplot2::ggplot()+ ggplot2::geom_line(ggplot2::aes(c(0,(1-tnr),1),c(0,tpr,1)),col="red")+
    ggplot2::geom_point(ggplot2::aes(1-tnr,tpr),col="red")+
    ggplot2::geom_line(ggplot2::aes(c(0,1),c(0,1)))+
    ggplot2::theme_classic()+
    ggplot2::scale_x_continuous(breaks = c(0,0.25,0.5,0.75,1),labels = c(1.00,0.75,0.50,0.25,0.00))+
    ggplot2::ggtitle("ROC Curve")+ggplot2::xlab("True negative rate")+ggplot2::ylab("True positive rate")
  res <- list("True positive rate"=tpr,
              "True negative rate"=tnr,
              "Area under the curve"=auc,
              "ROC curve"=roc)
  return(res)
}

