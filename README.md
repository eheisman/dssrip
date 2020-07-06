The DSS-R *i*nterface *p*roject. (dssrip)
=========================================

`dssrip` is a package of R functions to read/write data from HEC-DSS file by making calls to the Java methods exposed in HEC-DSSVue's scripting capability.

Provided under MIT license without warranty.

# Install Instructions:

## R libraries required
If you haven't already, install the `rJava`, `plyr`, `reshape2`, `stringr`, and `devtools` packages.  

## DSS libraries
You can download a copy of DSSVue or a number of other HEC programs that run on Java (most of them except for HEC-RAS) to get the required .jar and .dll files to make `dssrip` work.  I highly recommend you use the [copy here for 64-bit use](https://www.hec.usace.army.mil/software/hec-dssvue/downloads/dev/HEC-DSSVue-v3.0.00.212.7z).  `dssrip` is now packaged with a file called `jar_config.json` that helps find the appropriate libraries depending on the HEC program used to supply the .jar files.  If you're using another program, you can make a copy of this file, add a configuration for that program, and set the option `dss_config_filename` in your `.Rprofile` file to ensure `dssrip` finds the correct settings.

## `.Rprofile` settings required
Two settings need to be applied in your `.Rprofile` file to install and load `dssrip` correctly.  Without setting these, it falls back to the settings in the default `jar_config.json` file which may not work for your machine.  The install process for R packages happens in a clean environment, so setting them in your script prior to running install may not set them in that environment.

Setting the R option `dss_override_location` to the directory containing the `HEC-DSSVue.exe` file from the previously mentioned copy is the quickest way to get `dssrip` up and running.  

Setting the environment variable `JAVA_HOME` helps the `rJava` package that `dssrip` depends on find the correct Java executables.  For compatibility reasons, it should be pointed to the location of the Java Runtime included with the DSS libraries you are using.

My `.Rprofile` ends up looking like this:
```
if(R.Version()$arch=="x86_64"){
  # use 64-bit .jar and .dll
  options(dss_override_location="c:\\programs\\HEC-DSSVue-v3.0.00.212\\")
  Sys.setenv(JAVA_HOME=paste0(options("dss_override_location"), "java"))
} else {
  # use 32-bit .jar and .dll (old dssrip, no longer needed)
}
```

## Installing the package
Finally, to install `dssrip` , use the `devtools` package's `install_github` function.
```
devtools::install_github("eheisman/dssrip", INSTALL_opts = "--no-multiarch", ref="tidyup")
```

The ```'--no-multiarch'``` parameter is required to force it to install only the current architecture.  If you do not have the options set to point to a version of the javaHeclib.dll file with the same architecture as the version of R that you are running, the install will fail.  If you need to use both 64-bit and 32-bit R, you will have to install it once for each version.

## Note: for DSS installed elsewhere than Program Files
Set the following in your .Rprofile, or run before you install and/or load dssrip:
```
  options(dss_override_location="c:\\programs\\HEC-DSSVue-v3.0.00.212\\")
```
`dssrip` now automatically detects if the file separator character is needed on the end of this path, but including it is a safe option.

## Options to help with loading `dssrip`
Other `options` that can be used to help the package load correctly by pointing to the correct DSS .jar and library files:

- `dss_override_location` file path to force dssrip to load a particular set of dss libraries.  It will look for all configurations using only this location.
- `dss_config_filename` filename that points to a jar_config.json file external to the package's default configuration.
- `dss_allowed_states` list, defaults to `c('tested')`, filters config file to only allowed states.
- `dss_default_config`, string, forces to only use config going by this `name`.
- `dss_jvm_parameters` list passed to JVM when initialized in dssrip, allowing settings like memory usage to be altered or other JVM start-up options set.  This is not currently used.


# Usage:
Load ```dssrip``` as a library with `require(dssrip)` or `library(dssrip`.

`myFile = opendss(dssFilename)` to open a DSS file or to create a new one.  

From the returned `hecdss` object, either accompanying functions can be used to read data, or DSSVue's Jython API can be called.  See Chapter 8 of the DSS-Vue manual for a detailed list of Jython API calls.

Several convenience functions for reading timeseries and paried data containers have been added, a brief summary presented below:

`getPaths` - useful for getting the list of paths in a DSS file and searching through these paths by wildcards or regex strings.

`getFullTSC` - returns an `xts` object with the requested data.

`getFullDT` - similar to getFullTSC, but returns a data.table.

`getColumnsByName` - read a column from a PairedDataContainer.


# Known Issues:
- dssrip must be loaded before any other packages that require ```rJava``` so that dssrip can initialize a JVM with the correct options.  This isn't 'nice' behavior, but at the moment it is required.  This issue can be mostly resolved by loading dssrip before other packages that depend on ```rJava```, such as XLConnect. 
- Time series import does not handle timezones well, xts objects often default to assuming the file is in GMT.  This may be a larger issue with how R handles timezones on Windows.
- data.table and rJava both imported and have a naming conflict on the ```J()``` function.  At the moment the rJava version masks the data.table version, the reverse may be more useful.

# Hydroutils
The plotting and misc hydrology functions of this package has been moved to their own package, [hydroutils](http://github.com/eheisman/hydroutils).
