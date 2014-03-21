\name{plotBMA}
\alias{BMA-method}
\alias{plotBMA}
\alias{plotBMA,}
\title{Plot BMA class objects}
\usage{
plotBMA(x)
}
\arguments{
  \item{object}{A BMA class object}
}
\description{
Graphs the expected value distribution of coefficients for
BMA class objects
}
\examples{
myX <- matrix(rnorm(30, mean=10, sd=5), ncol=3)
myY <- sample(1:100, 10)
myBMA<- fitBMA(myX, myY)
plotBMA(myBMA)
}
\author{
Elif Ozdemir: \email{eozdemir@wustl.edu}
}
\seealso{
\code{\link{fitBMA}}

\code{\link{summaryBMA}}

\code{\link{plot2BMA}}
}

