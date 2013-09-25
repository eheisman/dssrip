DSS-Rip
=======
The DSS-R (i)nterface (p)roject

R functions to read/write data from HEC-DSS files.

Provided under MIT license without warranty.


Known Issues
============
Time series import does not handle timezones well, may have issues with DSS Vue's interpretation of 24:00 hours versus the XTS interpretation.  Check your data for off-by-one type errors!


Install Instructions
====================
```devtools::install_github("dss-rip","eheisman",args="--no-multiarch")```

'--no-multiarch' may be required as an option to resolve 32-bit/64-bit compatibility issues.


Usage
=====
Call ```initialize.dssrip()``` to setup the JVM with links to the HECLIB DLL files.  Then use ```myFile = opendss(dssFilename)``` to open your DSS file.  From the returned rJava 'hecdss' object, one call HEC-DSSVue Jython API calls, or use accompanying functiosn for a more R-like interface.
