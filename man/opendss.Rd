\name{opendss}
\alias{opendss}
\title{opendss Opens a DSS file.}
\usage{
opendss(filename, warnIfNew = TRUE, stopIfNew = FALSE)
}
\arguments{
  \item{filename}{Location of DSS file to open.}
}
\value{
'hec.heclib.dss.HecDss' object of DSS file at filename
}
\description{
Returns a DSS file object.
}
\details{
Returns an object from the java class
'hec.heclib.dss.HecDss' used for reading and writing to the
file located at filename.  Don't forget to call
myFile$close() or myFile$done() when finished.
}
\note{
NOTE
}
\author{
Evan Heisman
}

