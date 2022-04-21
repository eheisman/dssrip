Currently all development is on branch `main`.  Previous branches are described below.
# About branches
`dssrip` is being significantly rewritten to fix a number of issues with the latest versionss of `rJava` and `R` itself.  The existing branches are:

- `main`: The classic version, will be abandoned when `tidyup` is ready.
- `tidyup`: The current in-development version, not necessarily targeting `tidyverse` compatibility, but significantly improved to support `R 4.0`; removes some functionality to focus on reliability, only reading DSS data into a a couple of standard formats (`xts` and tidy `data.frame` for timeseries, wide `data.frame` for paired data).  `tidyup` supports expanded configurability for where it loads the DSS .jar files from in case different HEC libraries are available.
- `r36_rJava0.9-12fixes`: The classic version with fixes for `rJava 0.9-12`.  I only intend to make maintenance releases on this version related to compatibility with current `rJava` versions.
