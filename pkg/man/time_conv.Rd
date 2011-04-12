\name{time_conv}
\alias{time_conv}

\title{
 Simple Conversion of times 
}
\description{
  Gives the multipling factor to convert time between different units
}
\usage{
  time_conv(from, to)
}

\arguments{
  \item{from}{
  From what unit are you converting from? It can be:
  "second", "minute", "hour", "day", "week", "month", "year"
}
  \item{to}{
  To what unit are you converting to? It can be:
  "second", "minute", "hour", "day", "week", "month", "year"
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

time_conv("second", "minute")
time_conv("sec", "day") 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }