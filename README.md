The DSS-R *i*nterface *p*roject. (dssrip)
=========================================

R functions to read/write data from HEC-DSS file by making calls to the Java methods exposed in HEC-DSSVue's scripting capability.

NOTE: ```dssrip``` only works in 32-bit R, as it relies on a 32-bit JVM which calls JNI functions in a 32-bit DLL.

Provided under MIT license without warranty.

#Install Instructions:

If you haven't already, install the ```rJava```, ```ggplot2```,```plyr```, ```reshape2```, ```stringr```, and ```devtools``` packages.

To install, use the ```devtools``` package's ```install_github``` function:
```
devtools::install_github("eheisman/DSS-Rip",args="--no-multiarch")
```

```args='--no-multiarch'``` is required on a 64-bit computer to resolve 32-bit/64-bit compatibility issues.

Make sure a copy of DSSVue is installed to it's default location for your system.

#Usage:

Load ```dssrip``` as a library.  Yes, it's a different name than the github repository.

```myFile = opendss(dssFilename)``` to open a DSS file or to create a new one.  

From the returned ```hecdss``` object, either accompanying functions can be used to read data, or DSSVue's Jython API can be called.  See Chapter 8 of the DSS-Vue manual for a detailed list of Jython API calls.

Several convenience functions for reading timeseries and paried data containers have been added, a brief summary presented below:

```getPaths``` - useful for getting the list of paths in a DSS file and searching through these paths by wildcards or regex strings.

```getFullTSC``` - returns an ```xts``` object with the requested data.

```getFullDT``` - similar to getFullTSC, but returns a data.table.

```getColumnsByName``` - read PairedDataContainer to data.frame.

#Missing ```JAVA_HOME``` settings:
If you're trying to run DSSRip in 64-bit R, it will not work without a 64-bit javaHeclib.dll.

Otherwise, if you only have 64-bit Java on your system, you can set DSS-Rip to call the JRE bundled with HEC-DSSVue.

Set the following in your .Rprofile, or before you load dssrip:

```
options(dss_jre_location="C:\\Program Files (x86)\\HEC\\HEC-DSSVue\\java")
Sys.setenv(JAVA_HOME="C:\\Program Files (x86)\\HEC\\HEC-DSSVue\\java")
```
This fix may cause issues with other rJava based packages, so do this at your own risk.

#TODO List:
- Implement a 'safe' HecDss$get() method that handles a non-existent/'empty' path more gracefully, such as warn if null.
- Implement write methods for ```TimeSeriesContainers``` (from ```xts``` or ```data.frame```) and ```PairedDataContainers``` (from ```data.frame```).

#Known Issues:
- dssrip must be loaded before any other packages that require ```rJava``` so that dssrip can initialize a JVM with the correct options.  This isn't 'nice' behavior, but at the moment it is required.  This issue can be mostly resolved by loading dssrip before other packages that depend on ```rJava```, such as XLConnect.
- Only works in 32-bit R, unless you have a 64-bit version of the DLL file to link to.
- Time series import does not handle timezones well, xts objects often default to assuming the file is in GMT.  This may be a larger issue with how R handles timezones on Windows.
- data.table and rJava both imported and have a naming conflict on the ```J()``` function.  At the moment the rJava version masks the data.table version, the reverse may be more useful.

# Hydrotools
The Hydroltools part of this package has been moved to it's own package, [hydroutils](http://github.com/eheisman/hydroutils).
