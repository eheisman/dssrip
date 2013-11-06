The DSS-R *i*nterface *p*roject. (dssrip)
=========================================

R functions to read/write data from HEC-DSS files.

Provided under MIT license without warranty.

Install Instructions:
=====================
```
devtools::install_github("dss-rip","eheisman",args="--no-multiarch")
```

```args='--no-multiarch'``` is required on a 64-bit computer to resolve 32-bit/64-bit compatibility issues.

Make sure a copy of DSSVue is installed to it's default location for your system.

Usage:
======
```myFile opendss(dssFilename)``` to open a DSS file or to create a new one.  

From the returned ```hecdss``` object, either accompanying functions can be used to read data, or DSSVue's Jython API can be called.  See Chapter 8 of the DSS-Vue manual for a detailed list of Jython API calls.

TODO List:
==========
- Implement a 'safe' HecDss$get() method that handles a non-existent/'empty' path more gracefully, such as warn if null.
- Implement write methods for ```TimeSeriesContainers``` (from ```xts``` or ```data.frame```) and ```PairedDataContainers``` (from ```data.frame```).
- Classes that inherit from ```data.frame```/```data.table``` and maybe ```xts``` to hold DSS metadata.

Known Issues:
=============
- dssrip must be loaded before any other packages that require ```rJava``` so that dssrip can initialize a JVM with the correct options.
- Only known to work in 32-bit R.
- Time series import does not handle timezones well, may have issues with DSS-Vue's interpretation of 2400 hours versus the XTS interpretation.  Check your data for off-by-one type errors!
- data.table and rJava both imported and have a naming conflict on the ```J()``` function.  At the moment the rJava version masks the data.table version, the reverse may be more useful.