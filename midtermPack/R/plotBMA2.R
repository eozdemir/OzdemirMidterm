#' Plot BMA class objects
#'
#' Plots the posterior probability that the coefficient is non-zero for BMA class objects 
#' 
#' @usage plot2BMA(x) 
#' 
#' @param object A BMA class object
#'  
#' @author Elif Ozdemir: \email{eozdemir@@wustl.edu}
#'
#' @seealso \code{\link{fitBMA}}
#' @seealso \code{\link{summaryBMA}}
#' @seealso \code{\link{plotBMA}}
#' 
#' @rdname plot2BMA
#' @aliases plot2BMA, BMA-method
#' 
#' @examples
#' myX <- matrix(rnorm(30, mean=10, sd=5), ncol=3)
#' myY <- sample(1:100, 10)
#' myBMA<- fitBMA(myX, myY)
#' plot2BMA(myBMA)
#'
#'@export
setGeneric(name="plot2BMA",
           def=function(object)
           {standardGeneric("plot2BMA")}
)

#' @export
setMethod("plot2BMA", "BMA", 
          definition=function(object){
            par(mfrow=c(4,4))
            sapply(1:ncol(object@x), function(a){
              dplots<-  density(object@postProb[a])
              plot(dplots, main="Posterior probability that the coefficient is non-zero", xlim=c(min(dplots), max(dplots)))
            }) #end of function and sapply
          }) #end of function and setMethod