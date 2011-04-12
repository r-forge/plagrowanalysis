\name{covar}
\alias{covar}

\title{
 Population convariance
}
\description{
 Compute population covariance between x and y if these are vectors. If x and y are matrices then the covariances (or correlations) between the columns of x and the columns of y are computed
}
\usage{
covar(x, y, ...)
}

\arguments{
  \item{x}{
  a numeric vector, matrix or data frame.
}
  \item{y}{
 A vector, matrix or data frame with compatible dimensions to x.
}
  \item{\dots}{
 Other arguments to pass to \code{cov}
}
}
\details{
 This function is a wrapper of cov and will multiply its result \code{(length(x)-1)/length(x)}. 
}
\value{
Covariance
}
\references{
None 
}
\author{
 Luciano Selzer
}

\seealso{
 \code{\link[stats:cor]{cov}}
}
\examples{
x <- rnorm(20)
y <- rnorm(20)
covar(x, y)
}
\keyword{ univar }

