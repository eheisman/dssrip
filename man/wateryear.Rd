\name{wateryear}
\alias{wateryear}
\alias{wyday,}
\alias{wymonth,}
\alias{wymonth.abb}
\title{'lubridate' type functions for getting numeric values from `Date` and `POSIXt` objects.}
\usage{
wateryear(t)
}
\arguments{
  \item{t}{timestamp object (e.g. Date, POSIXt)}
}
\value{
integer of water year Works similar to 'year' function on
POSIXt classes
}
\description{
Gets water year (Oct to Sept) from POSIXt object.
}

