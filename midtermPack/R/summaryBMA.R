#' Summarize BMA class objects
#'
#' Prints summary info for BMA class objects 
#' 
#' @useage summaryBMA(x)
#'      
#' @param object A BMA class object
#' 
#' @author Elif Ozdemir: \email{eozdemir@@wustl.edu}
#' 
#' @seealso \code{\link{fitBMA}}
#' @seealso \code{\link{plotBMA}}
#' 
#' @rdname summaryBMA
#' @aliases summaryBMA, BMA-method
#'
#'@export
setGeneric(name="summaryBMA",
           def=function(object)
           {standardGeneric("summaryBMA")}
)

#' @export
setMethod("summaryBMA", "BMA", 
          definition=function(object){
            cat("Number of observations:", length(object@y), "\n")
            cat("Number of regressions:", length(object@coef), "\n")
            cat("Maximum R-squared:", max(object@Rsquare), "\n")
            cat("Minimum R-squared:", min(object@Rsquare), "\n")
            cat("Expected value of coefficients:", "\n")
              sapply(1:nrow(object@coef), function(x){
              cat(object@postExp[x],na.rm=TRUE,"\n")
              }) #end of function and sapply
            cat("Posterior probability that P(Beta!=0):", "\n")
              sapply(1:nrow(object@coef), function(x){
              cat(object@postProb[x],na.rm=TRUE,"\n")
              })#end of function and sapply
          })#end of function and setMethod