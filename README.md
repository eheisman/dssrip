The DSS-R *i*nterface *p*roject. (dssrip)
=========================================

R functions to read/write data from HEC-DSS file by making calls to the Java methods exposed in HEC-DSSVue's scripting capability.

NOTE: ```dssrip``` only works in 32-bit R, as it relies on a 32-bit JVM which calls JNI functions in a 32-bit DLL.

Provided under MIT license without warranty.

#Install Instructions:

To install, use the ```devtools``` package's ```install_github``` function:
```
devtools::install_github("eheisman/DSS-Rip",args="--no-multiarch")
```

```args='--no-multiarch'``` is required on a 64-bit computer to resolve 32-bit/64-bit compatibility issues.

Make sure a copy of DSSVue is installed to it's default location for your system.

#Usage:

Load ```dssrip``` as a library.  Yes, it's a different name than the github repository.

```myFile opendss(dssFilename)``` to open a DSS file or to create a new one.  

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
- dssrip must be loaded before any other packages that require ```rJava``` so that dssrip can initialize a JVM with the correct options.  This isn't 'nice' behavior, but at the moment it is required.
- Only works in 32-bit R due to limitations on the JVM loading 32-bit DLL files.
- Time series import does not handle timezones well, xts objects often default to assuming the file is in GMT.  This may be a larger issue with how R handles timezones on Windows.
- data.table and rJava both imported and have a naming conflict on the ```J()``` function.  At the moment the rJava version masks the data.table version, the reverse may be more useful.

# Hydrotools - not quite enough for it's own package.

Not specific for DSS file, but serveral useful functions for hydrologists are included:

```wateryear```, similar to the ```lubridate``` package's ```year``` function, returns the water year for a timestamp (starting 01 Oct of the previous calendar year). ```wymonth``` returns the month of the water year, starting with October, and the ```wy.month.abb``` constant contains the month abreviations in this order.

For assessing the fit of hydrologic models, the functions ```nash.sutcliff```, ```rmse```, and ```excelR2``` are provided.

```flowBreaks``` and ```probBreaks``` are provided for generating breaks on a plot with logarithmic and normal-deviate axes, such as those used for flow frequency graphics.  ```weibullProbs``` returns the corresponding Weibull plotting position for a given vector.

```hydro_flow_trans``` and ```hydro_prob_trans``` can be used with ```ggplot```'s ```scale_continuous``` for axes to automatically apply flowBreaks and probBreaks.

# An example:
annual_peaks_data.dss contains a record with a USGS flow frequency record.

```r
require(dssrip)
require(ggplot2)
require(scales)

testfile = opendss("C:/path/to/annual_peaks_data.dss")
peaks = getFullTSC(testfile, getPaths(testfile, "C=FLOW-*")[1])
colnames(peaks) = "FLOW"
peaks = fortify(peaks) ## xts to data.frame for ggplot2

ggplot(peaks, aes(y=FLOW, x=weibullProbs(FLOW))) + geom_point() + 
  theme_bw(base_size=11) + theme(legend.position = "bottom", panel.grid.minor=element_blank()) +
  scale_y_continuous(trans=hydro_flow_trans()) + 
  scale_x_continuous(trans=hydro_prob_trans(lines=c(1,2,5), labels=c(1,2,5), byPeriod=TRUE)) + 
  stat_smooth(method="glm", family=gaussian(link="log"))
```
