#' Plot BMA class objects
#'
#' Plots the expected value distribution of coefficients for BMA class objects 
#' 
#' @usage plotBMA(x) 
#' 
#' @param object A BMA class object
#'  
#' @author Elif Ozdemir: \email{eozdemir@@wustl.edu}
#'
#' @seealso \code{\link{fitBMA}}
#' @seealso \code{\link{summaryBMA}}
#' @seealso \code{\link{plot2BMA}}
#' 
#' @rdname plotBMA
#' @aliases plotBMA, BMA-method
#' 
#' @examples
#' myX <- matrix(rnorm(30, mean=10, sd=5), ncol=3)
#' myY <- sample(1:100, 10)
#' myBMA<- fitBMA(myX, myY)
#' plotBMA(myBMA)
#'
#'@export
setGeneric(name="plotBMA",
           def=function(object)
           {standardGeneric("plotBMA")}
)

#' @export
setMethod("plotBMA", "BMA", 
          definition=function(object){
            par(mfrow=c(4,4))
            sapply(1:ncol(object@x), function(a){
              dplots<-  density(object@postExp[a])
              plot(dplots, main="Expected value distribution of coefficients", xlim=c(min(dplots), max(dplots)))
            }) #end of function and sapply
            }) #end of function and setMethod