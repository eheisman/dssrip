\name{wyday}
\alias{wyday}
\title{Day of water year}
\usage{
wyday(t)
}
\arguments{
  \item{t}{timestamp object (e.g. Date, POSIXt)}
}
\value{
integer of day in water year (01Oct=1,
30Sep=365+leapyear(y))
}
\description{
Get the day of water year (01Oct=1, 30Sep=365+leapyear(y))
}

