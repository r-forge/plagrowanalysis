\name{print.gral}
\alias{print.gral}

\title{
 Prints grall objects
}
\description{
 Pretty printing of gral class
}
\usage{
print.gral(x, digits = max(3, getOption("digits") - 3), ...)
}

\arguments{
  \item{x}{
 a gral object 
}
  \item{digits}{
 Number of digits to print
}
  \item{\dots}{
 Extra options
}
}
\details{
 It's an internal function
}
\value{
 Return x invisibly
}

\author{
Luciano Selzer
}


\examples{

data(plant)
plant_rgr<- RGR_analysis("time", "total", data = plant,
  units = list(time=c("day", "day"), weight = c("mg","g"), area = c("mm2", "m2")))
print(plant_rgr)
}
\keyword{ internal }
