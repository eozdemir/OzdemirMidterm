#' Fitting regressions for BMA analysis
#'
#' BMA accounts for the model uncertainty inherent in the variable selection problem by averaging over the best models in the model class according to approximate posterior model probability
#'
#' @param x A matrix object
#' @param y A numeric object with the same number of elements as the number \code{x} rows.
#'
#' @return An object of class BMA containing
#'  \item{coef}{A coefficient matrix}
#'  \item{Rsquare}{A numeric vector}
#'  \item{postProb}{A numeric vector}
#'  \item{postModel}{A numeric vector}
#'  \item{postExp}{A numeric vector}
#'  \item{x}{The first object input} 
#'  \item{y}{The second object input}
#' @author Elif Ozdemir: \email{eozdemir@@wustl.edu}
#' 
#' @examples
#' myX <- matrix(rnorm(30, mean=10, sd=5), ncol=3)
#' myY <- sample(1:100, 10)
#' fitBMA(myX, myY)
#' 
#' @rdname fitBMA
#' @aliases fitBMA,ANY-method
#' @seealso \code{\link{plotBMA}}
#' @seealso \code{\link{summaryBMA}}
#' 
#' @export
setGeneric(name="fitBMA",
           def=function(x, y, g=4, parallel=FALSE)
           {standardGeneric("fitBMA")}
           )

#' @export
setMethod(f="fitBMA",
          definition=function(x, y, g=4, parallel=FALSE){ #the default will take g as 4 (i remember it can be 3 or 4 from class) and the parallel will not run unless the user specifies
          ##Standardization of covariates so that we won't need intercept
          #I used scale() function, thanks to google!  
          #http://stackoverflow.com/questions/15215457/standardize-data-columns-in-r
            scaled.x<- scale(x)
            scaled.y<- scale(y)  
          #The same code I used in the regressionPack problem set to regress for all covariate combinations/ without the intercept this time
            covCombn<- sapply(1:ncol(x), function(a){ #list for combination of covariates
            apply(combn(1:ncol(x),a), 2, function(b){
            tempCombn<- logical(length=ncol(x)) #assign empty logical and fill it with through the loop
            tempCombn[z]<- TRUE
            return(tempCombn)
          })#end of apply              
          })#end of supply
            covCombnMat<- matrix(unlist(covCombn), nrow=ncol(x)) #matrix of the covariate combinations
          #outputs of the function
            coefficients<- matrix(nrow=ncol(x), ncol=2^ncol(x)) #coefficient matrix
            r2<- vector(length=2^ncol(x)) #R-squared values to be calculated
            posteriorP<- vector(length=2^ncol(x)) #posterior probability to be calculated
            posteriorM<- vector(length=2^ncol(x)) #posterior model odds to be calculated
            posteriorE<- vector(length=2^ncol(x)) #posterior expected values to be calculated
          #fit the regression
            reg<- a_ply(.data=1:ncol(covCombn), .margins=1, .fun=function(c){
            model<- lm(scaled.y~scaled.x[,selectorMatrix[,c]]-1)
            coefficients[covCombn[,c],c]<- model[[1]]
            r2[c]<<- summary(model)[["r.squared"]]
            return(list(coefficients=coefficients[,c], r2=r2))
          }#end of function
          )#end of a_ply
            #check:not sure if it should be different when intercept removed
            #kept the problem set code I used before
            
            ##Calculation of the BMA formulas from the slides provided
            #Posterior model odds
            B.Mk.Mo<- sapply(1:ncol(coefficients), function(e){
              (1+g)^((nrow(x)-sum(!is.na(coefficients[1:ncol(x),e]))-1)/2)*(1+g*(1-r2[e]))^(-1*(nrow(x)-1)/2) #I calculated p_k as the number of covariates in each model although I am not sure what it stands for in this formula
            })#end of function and sapply
            posteriorM<- B.Mk.Mo/sum(B.Mk.Mo) #equation 10 in slides
            #Posterior excepted value of the models missing a particular covariate
            E.Bj.Mk<- (g/(g+1))*coefficients
            #equation 11 in slides            
            posteriorE<- apply(coefficients[1:ncol(x),], margin=1, fun=function(f){
              modCov<- which(is.na(f)) #models missing that covariate through the loop
              #using matrix algebra to sum the products of elements in matrices
              return(posteriorM[-modCov]%*%E.Bj.Mk[-modCov])
            }) #end of function and apply
            #Posterior probability that the coefficient is non-zero p(beta!=0)
            posteriorP<- apply(coefficients[1:nrow(x),], margin=1, fun=function(g){
              sum(posteriorM[-which(is.na(g))]) #excluding the given covariates through the loop for each model
            }) #end of function and apply
            return(new("BMA", coef=coefficients, Rsquare=r2, x=scaled.x, y=scaled.y, postProb=posteriorP, postModel=posteriorM, postExp=posteriorE))
          })