\name{make_periods}
\alias{make_periods}

\title{
 Make periods
}
\description{
 Creates a character vector containing the time span between times.
}
\usage{
make_periods(times, first_date = FALSE)
}

\arguments{
  \item{times}{
 a numeric vector.
}
  \item{first_date}{
    A logic value. \code{TRUE} uses the first date for the calculation. 
    Defaults to \code{FALSE}.
  }
}

\value{
 A character vector of lenght(times)-1
}

\author{
 Luciano Selzer
}


\examples{
make_periods(c(1,5,16,23))
}
\keyword{ internal}

