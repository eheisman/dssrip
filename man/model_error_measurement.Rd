\name{model_error_measurement}
\alias{excelR2}
\alias{model_error_measurement}
\alias{nash.sutcliffe}
\alias{rmse}
\title{Model error measurement metrics}
\usage{
\code{excelR2(x.obs, x.model)}
\code{rmse(x.obs, x.model)}
\code{rmse(residuals)}
\code{nash.sutcliffe(x.obs, x.model)}
\code{nash.sutcliffe(x.obs, x.model, x.alt)}
}
\arguments{
  \item{x.obs}{- observed values}

  \item{x.model}{- modeled values}

  \item{x.alt}{- alternate model for comparison (Nash
  Sutcliffe only, defaults to mean of observed)}
}
\description{
Common interface for metrics commonly for assessing quality
of hydrologic models, statistical or otherwise.
}
\details{
\code{excelR2} returns the R^2 as reported by Excel's curve
fits.  It is provided as people are comfortable with it,
but is a terrible measure of model accuracy as it only
provides correlation, not fit.

\code{rmse} returns the root mean square error.  With one
parameter assumes x.obs is residuals, with two, computes
residuals between x.obs and x.model

\code{nash.sutcliffe} returns the Nash-Sutcliffe model
coefficent, greater than 0 if `x.model` is a better fit
than x.alt, 1 if a perfect fit, and between 0 and -infinity
if a worse fit than x.alt.
}
\author{
Evan Heisman
}
\references{
Nash, J. E. and J. V. Sutcliffe (1970), River flow
forecasting through conceptual models part I - A discussion
of principles, Journal of Hydrology, 10 (3), 282–290.
}

