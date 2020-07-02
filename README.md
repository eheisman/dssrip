The DSS-R *i*nterface *p*roject. (dssrip)
=========================================

R functions to read/write data from HEC-DSS file by making calls to the Java methods exposed in HEC-DSSVue's scripting capability.

NOTE: ```dssrip``` works best in 32-bit R with DSSVue 2.0 from the HEC website.  Setting it up with in other configurations (64-bit R, newer version of the DSS libraries) is at your own risk.

Provided under MIT license without warranty.

# Install Instructions:

If you haven't already, install the ```rJava```, ```ggplot2```,```plyr```, ```reshape2```, ```stringr```, and ```devtools``` packages.  Before installing ```dssrip```, make sure a copy of DSSVue is installed to the default location for your system or configure your `.rprofile` file with the options set below.

To install, use the ```devtools``` package's ```install_github``` function.

For newer versions of R:
```
devtools::install_github("eheisman/dssrip", INSTALL_opts = "--no-multiarch", ref="tidyup")
```
For older versions of R (at least 3.4 and older):
```
devtools::install_github("eheisman/dssrip", args="--no-multiarch", ref="tidyup")
```

The ```'--no-multiarch'``` parameter is required on a 64-bit computer to force it to install only the current architecture.  If you do not have the options set to point to a version of the javaHeclib.dll file with the same architecture as the version of R that you are running, the install will fail.  If you need to use both 64-bit and 32-bit R, you will have to install it once for each version.

# Usage:

Load ```dssrip``` as a library with ```require``` or ```library```.

```myFile = opendss(dssFilename)``` to open a DSS file or to create a new one.  

From the returned ```hecdss``` object, either accompanying functions can be used to read data, or DSSVue's Jython API can be called.  See Chapter 8 of the DSS-Vue manual for a detailed list of Jython API calls.

Several convenience functions for reading timeseries and paried data containers have been added, a brief summary presented below:

```getPaths``` - useful for getting the list of paths in a DSS file and searching through these paths by wildcards or regex strings.

```getFullTSC``` - returns an ```xts``` object with the requested data.

```getFullDT``` - similar to getFullTSC, but returns a data.table.

```getColumnsByName``` - read a column from a PairedDataContainer.

## For DSS installed elsewhere than Program files
Set the following in your .Rprofile, or run before you install and/or load dssrip:
```
  options(dss_override_location="c:\\programs\\HEC-DSSVue-v3.0.00.212\\")
```

Other `options` that can be used to help the package load correctly by pointing to the correct DSS .jar and library files:

- `dss_jvm_parameters` string passed to JVM when initialized, allowing memory settings to be altered or other JVM start-up options set.  This is untested.
- `dss_override_location` file path to force dssrip to load a particular set of dss libraries.  It will look for all configurations using only this location.
- `dss_config_filename` filename that points to a jar_config.json file external to the package's default configuration.
- `dss_allowed_states` list, defaults to `c('tested')`, filters config file to only allowed states.
- `dss_default_config`, string, forces to only use config going by this `name`.


# TODO List:
- Implement a 'safe' HecDss$get() method that handles a non-existent/'empty' path more gracefully, such as warn if null.
- Implement write methods for ```TimeSeriesContainers``` (from ```xts``` or ```data.frame```) and ```PairedDataContainers``` (from ```data.frame```).

# Known Issues:
- dssrip must be loaded before any other packages that require ```rJava``` so that dssrip can initialize a JVM with the correct options.  This isn't 'nice' behavior, but at the moment it is required.  This issue can be mostly resolved by loading dssrip before other packages that depend on ```rJava```, such as XLConnect.
- Only works in 32-bit R, unless you have a 64-bit version of the DLL file to link to, such as those provided with the [HEC-WAT](https://www.hec.usace.army.mil/software/hec-wat/) software.
- Time series import does not handle timezones well, xts objects often default to assuming the file is in GMT.  This may be a larger issue with how R handles timezones on Windows.
- data.table and rJava both imported and have a naming conflict on the ```J()``` function.  At the moment the rJava version masks the data.table version, the reverse may be more useful.

# Hydroutils
The plotting and misc hydrology functions of this package has been moved to their own package, [hydroutils](http://github.com/eheisman/hydroutils).
