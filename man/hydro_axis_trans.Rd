\name{hydro_axis_trans}
\alias{flowBreaks}
\alias{hydro_axis_trans}
\alias{hydro_flow_breaks}
\alias{hydro_flow_trans}
\alias{hydro_prob_breaks}
\alias{hydro_prob_trans}
\alias{probBreaks}
\title{Hydrologic axis transforms for ggplot2}
\usage{
hydro_prob_trans(distribution = "norm", distArgs = list(), ...)
}
\description{
Axis transforms for hydrologic plots in ggplot2.
}
\details{
\code{hydro_prob_trans} and \code{hydro_prob_breaks} for
probability axes on frequency plots

\code{hydro_flow_trans} and \code{hydro_flow_breaks} for
log axes with additional breaks
}
\examples{
ggplot(peaks, aes(y=PEAK_DAILY_FLOW, x=weibullProbs(PEAK_DAILY_FLOW))) + geom_point() +
  theme_bw(base_size=11) + theme(legend.position = "bottom", panel.grid.minor=element_blank()) +
  scale_y_continuous(trans=hydro_flow_trans()) +
  scale_x_continuous(trans=hydro_prob_trans(lines=c(1,2,5), labels=c(1,2,5), byPeriod=TRUE)) +
  stat_smooth(method="glm", family=gaussian(link="log"))
}
\author{
Evan Heisman
}

