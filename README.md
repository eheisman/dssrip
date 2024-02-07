The DSS-R *i*nterface *p*roject. (dssrip)
=========================================

*The use of the `dssrip` package is not endorsed by the Hydrologic Engineering Center (HEC) and the Hydrologic Engineering Center cannot guarantee the results of using this package.  Do not expect support or assistance with the use of this package, and there are no plans for future updates to this package.*


`dssrip` is a package of R functions to read data from HEC-DSS file by making calls to the Java methods exposed in HEC-DSSVue's scripting capability.

Provided under MIT license without warranty. 

# Install Instructions:

## R packages required
If you haven't already, install the `rJava`, `plyr`, `reshape2`, `stringr`, `xts`, `broom`, and `rjson` packages that are required by the `dssrip` package's code.  In addition, `remotes` (previously `devtools`) is recommended to install directly from github.

## DSS libraries
As of the 0.4 version of `dssrip`, the package will download it's own dependencies during the installation process (or the first time it is run, if manually installed).  

To make this work, the following options need to be set prior to installation:
- `dss_override_location`: where do you want the dependencies to go.  This directory should exist and contain two sub-directories, "jar" and "lib", otherwise the dependency download via `curl` will fail.
- `dss_config_filename`: location of a .json file spelling out required dependencies and JVM location, see example in `config` directory of this repository.

I make this work with the following in my `.Rprofile`:
```
# this line tells dssrip where to store / find the dependencies, you must be able to load a .dll from this location.
options(dss_override_location="c:\\projects\\dssrip\\monolith")
# this points to a config file that isn't the default used by the package.  I recommend doing this manual step.
options(dss_config_filename="~\\dssrip.config")
# This config is the current prefered method of using dssrip
options(dss_default_config="monolith-win-x86_64")
# required if default config is not a "tested" configuration.
options(dss_allowed_states="untested") 
options(dssrip_debug=T)
```

## Installing the package
After configuring as above, to install `dssrip`, use the `remotes` package's `install_github` function.
```
remotes::install_github("eheisman/dssrip", INSTALL_opts = "--no-multiarch", ref="main")
```

The ```'--no-multiarch'``` parameter is required to force it to install only the current architecture.  If you do not have the options set to point to a version of the javaHeclib.dll file with the same architecture as the version of R that you are running, the install will fail.  If you need to use both 64-bit and 32-bit R, you will have to install it once for each version.


## *Deprecated*: `.Rprofile` settings required for using DSSVue as dependencies.
Two settings need to be applied in your `.Rprofile` file to install and load `dssrip` correctly.  Without setting these, it falls back to the settings in the default `jar_config.json` file which may not work for your machine.  The install process for R packages happens in a clean environment, so setting them in your script prior to running install may not set them in that environment.

Setting the environment variable `JAVA_HOME` helps the `rJava` package that `dssrip` depends on find the correct Java executables.  For compatibility reasons, it should be pointed to the location of the Java Runtime included with the DSS libraries you are using.

An example `.Rprofile` for this configuration ends up looking like this:
```
if(R.Version()$arch=="x86_64"){
  # use 64-bit .jar and .dll
  options(dss_override_location="c:\\programs\\HEC-DSSVue-v3.0.00.212\\")
  Sys.setenv(JAVA_HOME=paste0(options("dss_override_location"), "java"))
} else {
  # use 32-bit .jar and .dll (old dssrip, no longer needed)
}
```

### Note: for DSS installed elsewhere than Program Files
Set the following in your .Rprofile, or run before you install and/or load dssrip:
```
  options(dss_override_location="c:\\programs\\HEC-DSSVue-v3.0.00.212\\")
```
`dssrip` now automatically detects if the file separator character is needed on the end of this path, but including it is a safe option.

## R `options` to modify the process to load DSS dependencies for `dssrip`
Other `options` that can be used to help the package load correctly by pointing to the correct DSS .jar and library files:

- `dss_override_location` file path to force dssrip to load a particular set of dss libraries.  It will look for all configurations using only this location.
- `dss_config_filename` filename that points to a jar_config.json file external to the package's default configuration.
- `dss_allowed_states` list, defaults to `c('tested')`, filters config file to only allowed states.
- `dss_default_config`, string, forces to only use config going by this `name`.
- `dss_jvm_parameters` list passed to JVM when initialized in dssrip, allowing settings like memory usage to be altered or other JVM start-up options set.  This is not currently used.
- `dssrip_debug` boolean sets debug print statements on or off.

# Usage:
Load ```dssrip``` as a library with `require(dssrip)` or `library(dssrip)`.

`myFile = opendss(dssFilename)` to open a DSS file or to create a new one.  

From the returned `hecdss` object, either accompanying functions can be used to read data, or DSSVue's Jython API can be called.  See Chapter 8 of the DSS-Vue manual for a detailed list of Jython API calls.

Several convenience functions for reading timeseries and paried data containers have been added, a brief summary presented below:

`getPaths` - useful for getting the list of paths in a DSS file and searching through these paths by wildcards or regex strings.

`getFullTSC` - returns an `xts` object with the requested data.

`getColumnsByName` - read a column from a PairedDataContainer.


# `jar_config.json` file:
The following is an example of the `jar_config.json` file required to point `dssrip` at a different copy of the DSS libraries.
```
{
  "configs": [
    {
      "name": "dssvue3-win64",
      "state": "tested",
      "notes": "this is the best supported configuration",
      "platform": "x86_64-w64-mingw32",
      "path.sep": "\\",
      "dss_location": "C:\\Programs\\HEC-DSSVue-v3.0.00.212",
      "jars": ["jar\\hec.jar", "jar\\rma.jar", "jar\\lookup.jar", "jar\\hec-dssvue-v3.0.jar", "jar\\help\\dssvueHelp.jar"],
      "libs": ["lib"],
      "lib.ext": "dll",
      "JAVA_HOME": "java"
    }
  ],
  "default_config":"dssvue3-win64"
}
```

##Fields under the `configs` blocks:
- `name`: name for the configuration
- `state`: if `tested` will be used unless other states are allowed.
- `notes`: metadata for describing the config
- `platform`: must match the R platform to use this state.
- `path.sep`: really should be tied to platform
- `dss_location`: root directory for the program providing the DSS libraries
- `jars`: list of jar files, relative to `dss_location` that need to be loaded.
- `libs`: directory of native library files needed
- `lib.ext`: another one that should be tied to platform; the extension of the native library files.
- `JAVA_HOME`: the directory containing the JVM executables to be used.

`default_config`: if multiple match, use the one with this name by default.

# Known Issues:
- dssrip must be loaded before any other packages that require ```rJava``` so that dssrip can initialize a JVM with the correct options.  This isn't 'nice' behavior, but at the moment it is required.  This issue can be mostly resolved by loading dssrip before other packages that depend on ```rJava```, such as XLConnect. 
- Time series import does not handle timezones well, xts objects often default to assuming the file is in GMT.  This may be a larger issue with how R handles timezones on Windows.

# Hydroutils
The plotting and misc hydrology functions of this package has been moved to their own package, [hydroutils](http://github.com/eheisman/hydroutils).
