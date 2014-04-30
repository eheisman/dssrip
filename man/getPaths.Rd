\name{getPaths}
\alias{getPaths}
\title{getPaths Search DSS paths by filter.}
\usage{
getPaths(file, searchString = "/*/*/*/*/*/*/", searchfunction = NULL,
  pattern = searchString, searchFunction = searchfunction,
  useRegex = FALSE)
}
\arguments{
  \item{file}{DSS file reference}

  \item{searchString}{Search string}

  \item{searchFunction}{Filter function to use with search
  string}
}
\value{
character vector of paths matching filter criteria.
}
\description{
Allows searching DSS paths similar to
getCatalogedPathnames(searchPattern) in the Jython API.
}
\details{
Uses the pattern parameter to return a filtered list of
paths.  The filter method is defined by the searchfunction
parameter.
}
\note{
NOTE
}
\author{
Evan Heisman
}

