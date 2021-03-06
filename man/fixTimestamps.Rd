% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timeSeriesContainers.R
\name{fixTimestamps}
\alias{fixTimestamps}
\title{Adjusts timestamps back by one day.}
\usage{
fixTimestamps(ts, n = -24 * 60 * 60)
}
\arguments{
\item{n}{seconds to adjust timestamp by, defaults to -24*60*60, e.g. 1 day.}
}
\value{
xts with timestamps adjusted back by n
}
\description{
Used to convert daily data's 24:00 timestamps back to proper date.
}
\details{
This should be fixed by passing "offsetByInterval" to the `tsc.to.xts` function as that is more robust to timesteps other than daily.
}
\note{
- this should be automated in future version by checking if the TSC's type allows 24:00 timestamps
}
