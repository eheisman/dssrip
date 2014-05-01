\name{hydrologic_date_functions}
\alias{hydrologic_date_functions}
\alias{wateryear}
\alias{wyday}
\alias{wymonth}
\alias{wymonth.abb}
\title{Hydrologic date helper functions}
\usage{
wateryear(t)
}
\arguments{
  \item{t}{timestamp object (e.g. \code{Date},
  \code{POSIXt} classes)}
}
\value{
\code{wateryear} water year for a given timestamp (October
of previous calendar year through September)

\code{wymonth} month of water year (Oct = 1, Sept = 12)

\code{wyday} day of water year (01Oct=1,
30Sep=365+leapyear(y))
}
\description{
Provides 'lubridate' type functions for getting numeric
values from `Date` and `POSIXt` objects based on the water
year.
}
\note{
\code{wy} is no longer provided due to conflicts with the
common use as a variable name in loops
}
\author{
Evan Heisman
}

