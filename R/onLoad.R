## onLoad.R
## Calls initialize.dssrip() to set up JVM, and add required DLLs.

.onLoad <- function(libname, pkgname){
  #initialize.dssrip(pkgname=NULL, lib.loc=libname, quietDSS=T) #, parameters=options("java.parameters"))
  initialize.dssrip(pkgname=pkgname, lib.loc=libname, quietDSS=T) ## Option 2 for ignoring previous JVM. DOES NOT WORK

  # cache a bunch of metadata used to help parse HEC's Java objects
  javaObjs = new.env()
  assign("hec.io.TimeSeriesContainer", fieldsDF(.jnew("hec/io/TimeSeriesContainer")), javaObjs)
  assign("hec.io.PairedDataContainer", fieldsDF(.jnew("hec/io/PairedDataContainer")), javaObjs)
  assign("hecJavaObjectsDB", javaObjs, envir=parent.env(environment()))
  assign("DSS_CONSTANTS", J("hec/script/Constants"), envir=parent.env(environment()))
  assign("TSC_INTERVALS", TSC_INTERVALS, envir=parent.env(environment()))
  assign("TSC_TYPES", TSC_TYPES, envir=parent.env(environment()))
}