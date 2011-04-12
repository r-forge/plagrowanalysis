\name{weight_conv}
\alias{weight_conv}

\title{
 Simple Conversion of SI weights
}
\description{
  Gives the multipling factor to convert weight between different units
}
\usage{
weight_conv(from, to)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{from}{
  From what unit are you converting from? It can be:
  "ug", "mg", "g", "kg", "tonne"
}
  \item{to}{
  To what unit are you converting to? It can be:
  "ug", "mg", "g", "kg", "tonne"
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
 
 weight_conv("ug", "g")
 weight_conv("kg", "g")
}
\keyword{ manip }
