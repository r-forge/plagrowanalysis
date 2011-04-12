\name{area_conv}
\alias{area_conv}

\title{
 Simple Conversion of SI Areas 
}
\description{
  Gives the multipling factor to convert an area between different units
}
\usage{
  area_conv(from, to)
}

\arguments{
  \item{from}{
  From what unit are you converting from? It can be:
  "mm2", "cm2", "dm2", "m2", "a", "ha", "km2"
}
  \item{to}{
  To what unit are you converting to? It can be:
  "mm2", "cm2", "dm2", "m2", "a", "ha", "km2"
}
}
\details{
 The function can accept abbreviations. Given they can be individualized,
  e.g. m for minutes will not work buy mi will.
}
\value{
  \item{mult}{multiplier to multiply the original value}
}
\references{
 International System of Units: http://en.wikipedia.org/wiki/International_System_of_Units
}
\author{
 Luciano Selzer
}


\examples{

area_conv("mm2", "cm2") 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }