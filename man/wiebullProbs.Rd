\name{wiebullProbs}
\alias{wiebullProbs}
\usage{
weibullProbs(Qs, exceedance = FALSE, as.points = FALSE)
}
\arguments{
  \item{Qs}{list of points for which weibull plotting
  positions are needed}

  \item{exceedance}{boolean that determines if plotting
  positions should be exceedance probabilities or
  non-exceedance probabilities.}

  \item{as.points}{boolean if ties should get independent
  points or not.}
}
\value{
probabilities in range of (0,1) corresponding to each point
in input vector.
}
\description{
generates weibull plotting positions for given vector.
Often used in hydrologic 'flow frequency curves'
}

