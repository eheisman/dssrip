\name{model_error_measurement}
\alias{excelR2}
\alias{model_error_measurement}
\alias{nash.sutcliffe,}
\alias{rmse,}
\title{Model error measurement functions}
\usage{
excelR2(x.obs, x.model)
}
\arguments{
  \item{x.obs}{- observed values}

  \item{x.model}{- modeled values}

  \item{x.alt}{- alternate model for comparison (Nash
  Sutcliffe only, defaults to mean of observed)}
}
\value{
score - see details.
}
\description{
Model error measuring functions
}
\details{
`excelR2` returns the R^2 as reported by Excel's curve
fits. `rmse` returns the root mean square error.
`nash.sutcliffe` returns the Nash-Sutcliffe model
coefficent, greater than 0 if `x.model` is a better fit
than x.alt, 1 if a perfect fit, and between 0 and -\Inf if
a worse fit than x.alt.
}

