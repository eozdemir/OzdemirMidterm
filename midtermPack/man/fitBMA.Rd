\name{fitBMA}
\alias{fitBMA}
\alias{fitBMA,ANY-method}
\title{Fitting regressions for BMA analysis}
\usage{
fitBMA(x, y, g = 4, parallel = FALSE)
}
\arguments{
  \item{x}{A matrix object}

  \item{y}{A numeric object with the same number of
  elements as the number \code{x} rows.}
}
\value{
An object of class BMA containing \item{coef}{A coefficient
matrix} \item{Rsquare}{A numeric vector} \item{postProb}{A
numeric vector} \item{postModel}{A numeric vector}
\item{postExp}{A numeric vector} \item{x}{The first object
input} \item{y}{The second object input}
}
\description{
BMA accounts for the model uncertainty inherent in the
variable selection problem by averaging over the best
models in the model class according to approximate
posterior model probability
}
\examples{
myX <- matrix(rnorm(30, mean=10, sd=5), ncol=3)
myY <- sample(1:100, 10)
fitBMA(myX, myY)
}
\author{
Elif Ozdemir: \email{eozdemir@wustl.edu}
}
\seealso{
\code{\link{plotBMA}}

\code{\link{summaryBMA}}
}

