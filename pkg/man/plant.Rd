\name{plant}
\alias{plant}
\docType{data}
\title{
 Plant biomass
}
\description{
 Test data, biomass and leaf area.
}
\usage{data(plant)}
\format{
  A data frame with 34 observations on the following 6 variables.
  \describe{
    \item{\code{time}}{a numeric vector. Time in days.}
    \item{\code{root}}{a numeric vector. Root biomass in mg.}
    \item{\code{non.leaf}}{a numeric vector. Non.leaf biomass in mg.}
    \item{\code{leaf}}{a numeric vector. Leaf biomass in mg.}
    \item{\code{leaf.area}}{a numeric vector. Leaf area in mm2.}
    \item{\code{total}}{a numeric vector. Total biomass in mg}
  }
}

\source{
 The data from the 7th and 21th days are from the paper cited here. 45th day
 is made up random data.
}
\references{
Hunt, R., Causton, D.R., Shipley, B. & Askew, A.P. (2002) A Modern Tool for Classical Plant Growth Analysis. Annals of Botany, 90, 485-488.
}
\examples{
data(plant)
## maybe str(plant) ; plot(plant) ...
}
\keyword{datasets}
