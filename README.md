DSS-Rip
=======
The DSS-R (i)nterface (p)roject

R functions to read/write data from HEC-DSS files.  This may become a more formal R package, when I have time.

At the moment, the loading of the package, or installing from github using devtools, does not work.  The best way to use this "package" is to download and 'source' the dssrt.r file to load the functions.

Provided under MIT license without warranty.


Known Issues
============
Time series import does not handle timezones well, may have issues with DSSVue's interpretation of 24:00 hours versus the XTS interpretation.  Check your data for off-by-one type errors!
