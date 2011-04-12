\name{RGR}
\alias{RGR}
\alias{ULR}
\alias{LAR}
\alias{LSA}
\alias{LWR}
\alias{RSA}

\title{
 Compute different growth parameters
}
\description{
 This set of function are the workhorse to calculate different growth parameters:   Relative Growth Rate, Unity leaf Rate, leaf Area Rate, leaf Specific Area, leaf Weight Ratio, Root - Shoot Allometry
}
\usage{
RGR(W1, W2, T1, T2)

ULR(W1, W2, L1, L2, T1, T2)

LAR(W1, W2, L1, L2)

LSA(L1, L2, LW1, LW2)

LWR(W1, W2, LW1, LW2)

RSA(R1, R2, LW1, LW2, S1, S2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{W1}{
 total weight at time 1.
}
  \item{W2}{
 total weight at time 2.
}
  \item{T1}{
 Time 1.
}
  \item{T2}{
 Time 2.
}
  \item{L1}{
 leaf area at time 1.
}
  \item{L2}{
 leaf area at time 2.
}
  \item{LW1}{
 leaf weight at time 1.
}
  \item{LW2}{
 leaf weight at time 2.
}
  \item{R1}{
 Root weight at time 1.
}
  \item{R2}{
 Root weight at time 2.
}
  \item{S1}{
 Shoot weight at time 1.
}
  \item{S2}{
 Shoot weight at time 2.
}
}
\details{
 This set of functions are internal are not meant
 to be used directly by the user.
}
\value{
  A matrix with the following elements:
       \item{value}{value of the parameter.}
       \item{Std err}{Standard Error of the parameter.}
       \item{95\% CL}{95\% confidence interval.}
}
\references{
Hunt, R., Causton, D.R., Shipley, B. & Askew, A.P. (2002) A Modern Tool for Classical Plant Growth Analysis. Annals of Botany, 90, 485-488.
}
\author{
Luciano Selzer
}


\seealso{
\code{\link{RGR_analysis}\link{growth_analysis}}

}
\examples{
data(plant)

data_t1 <- subset(plant, time == 7)
data_t2 <- subset(plant, time == 21)

RGR(data_t1$total, data_t2$total, 7, 21)

ULR(data_t1$total, data_t2$total,  data_t1$leaf.area,  data_t2$leaf.area, 7, 21 )
  
LAR(data_t1$total, data_t2$total,data_t1$leaf.area, data_t2$leaf.area)

LWR(data_t1$total, data_t2$total, data_t1$leaf, data_t2$leaf)

LSA(data_t1$leaf.area, data_t2$leaf.area,  data_t1$leaf, data_t2$leaf)

RSA(data_t1$root, data_t2$root, data_t1$leaf, data_t1$leaf, 
  data_t1$non.leaf, data_t2$non.leaf)

}
\keyword{ internal }
\keyword{ manip }
