\name{plot2BMA}
\alias{BMA-method}
\alias{plot2BMA}
\alias{plot2BMA,}
\title{Plot BMA class objects}
\usage{
plot2BMA(x)
}
\arguments{
  \item{object}{A BMA class object}
}
\description{
Graphs the posterior probability that the coefficient is
non-zero for BMA class objects
}
\examples{
myX <- matrix(rnorm(30, mean=10, sd=5), ncol=3)
myY <- sample(1:100, 10)
myBMA<- fitBMA(myX, myY)
plot2BMA(myBMA)
}
\author{
Elif Ozdemir: \email{eozdemir@wustl.edu}
}
\seealso{
\code{\link{fitBMA}}

\code{\link{summaryBMA}}

\code{\link{plotBMA}}
}

