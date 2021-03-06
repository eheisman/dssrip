% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dssrip2.r
\name{initialize.dssrip}
\alias{initialize.dssrip}
\title{dssrip initialization}
\usage{
initialize.dssrip(
  pkgname = NULL,
  quietDSS = TRUE,
  parameters = options()[["dss_jvm_parameters"]],
  setJavaLoc = FALSE,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{quietDSS}{- if true, don't show 'Z' messages during opening, reading, and writing to a file.}

\item{parameters}{list of options string to pass to JVM. (defaults to the option `dss_jvm_parameters` or NULL)}

\item{setJavaLoc}{- override Java location with one in config file. (untested)}

\item{verbose}{- set to true for debuggering}
}
\value{
JVM initialization status - 0 if successful, positive for partial initialization, negative for failure.  See ?.jinit
}
\description{
Starts a rJava JVM with configuration for dss's jar and dll files.
}
\details{
This method is typically called by the onLoad function when the package is imported.
List of `options` can be passed to this package via the R global `options()` function:
- `dss_jvm_parameters` string passed to JVM when initialized, allowing memory settings to be altered or other JVM start-up options set.  This is untested.
- `dss_override_location` file path to force dssrip to load a particular set of dss libraries.  This is untested. 
- `dss_config_filename` filename that points to a jar_config.json file external to the package's default configuration.
- `dss_allowed_states` list, defaults to `c('tested')`, filters config file to only allowed states.
- `dss_default_config`, string, forces to only use config going by this `name`.
}
\seealso{
loadConfig
}
\author{
Evan Heisman
}
