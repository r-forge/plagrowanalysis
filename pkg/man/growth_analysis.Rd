\name{growth_analysis}
\alias{growth_analysis}
\alias{RGR_analysis}
\alias{ULR_analysis}
\alias{LAR_analysis}
\alias{LSA_analysis}
\alias{LWR_analysis}
\alias{RSA_analysis}

\title{
 Analize data to obtain growth parameters.
}
\description{
 These functions calculate growth parameters for a
 set of data.
}
\usage{
  growth_analysis(time, root, leaf, non.leaf, leaf.area, total, data, 
    units = NULL)
  RGR_analysis(time, total, data, units = NULL)
  ULR_analysis(time, total, leaf.area, data, units = NULL)
  LAR_analysis(time, total, leaf.area, data, units = NULL)
  LWR_analysis(time, total, leaf, data, units = NULL)
  LSA_analysis(time, leaf, leaf.area, data, units = NULL)
  RSA_analysis(time, root, leaf, non.leaf, data, units = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{time}{
    Quoted name of the time variable in the data.
}
  \item{root}{
    Quoted name of the root weight variable in the data.
}
  \item{leaf}{
    Quoted name of the leaf weight variable in the data.
}
  \item{non.leaf}{
    Quoted name of the non.leaf weight variable in the data.
}
  \item{leaf.area}{
    Quoted name of the leaf area variable in the data.
}
  \item{total}{
     Quoted name of the total weight variable in the data.
}
  \item{data}{
    A \code{data.frame} where the data is.
}
  \item{units}{
    A list that can contain three items: \code{time}, 
      \code{weight}, \code{area}. Each object contains the units
      in which the data is and the desired units. See \link{area_conv}
      for more info.
}
}
\details{
 These functions calculate different growth parameters. \code{growth_analysis}
 is a wrapper that will calculate all the parameters at once.
}
\value{
If growth_analysis was called then a list with the different parameters is returned.
If any of the individual functions is called, then a list with the following items:
  \item{ans}{A matrix containing the value, standard error and 95\% confidence 
  interval.}
  \item{units}{A character vector containing the units in which the values are.}
  \item{lname}{A character vector conatining the long name of the growth parameter.}
}
\references{
Hunt, R., Causton, D.R., Shipley, B. & Askew, A.P. (2002) A Modern Tool for Classical Plant Growth Analysis. Annals of Botany, 90, 485-488.
}
\author{
Luciano Selzer
}


\examples{
data(plant)

growth_analysis("time", "root", "leaf", "non.leaf", "leaf.Area", data = plant)

growth_analysis("time", "root", "leaf", "non.leaf", "leaf.Area", data = plant,
  units = list(time=c("day", "day"), weight = c("mg","g"), area = c("mm2", "m2")))

RGR_analysis("time", "total", data = plant,
  units = list(time=c("day", "day"), weight = c("mg","g"), area = c("mm2", "m2")))
ULR_analysis("time", "total", "leaf.Area", data = plant)
}
\keyword{ manip }
