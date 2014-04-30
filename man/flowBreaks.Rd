\name{flowBreaks}
\alias{flowBreaks}
\usage{
flowBreaks(Q, labels = c(1, 2, 3, 5, 7))
}
\arguments{
  \item{Q}{flows, used to determine range}

  \item{labels}{which breaks should be labeled, using
  seq(1,9) would be crowded, default is usually
  acceptable.}
}
\value{
list of breaks with names to use as labels.
}
\description{
Generates a list of breaks for log10 axes that includes
multiples of each magnitude, with names from 'labels'.
}

