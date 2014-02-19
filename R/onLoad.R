## onLoad.R
## Calls initialize.dssrip(as.package=T) to set up JVM, and add required DLLs.

.onLoad <- function(libname, pkgname){
  initialize.dssrip(pkgname=NULL, lib.loc=libname, quietDSS=T)
  #initialize.dssrip(pkgname=pkgname, lib.loc=libname, quietDSS=T) ## Option 2 for ignoring previous JVM. DOES NOT WORK

  javaObjs = new.env()
  assign("tscFieldsDF", fieldsDF(.jnew("hec/io/TimeSeriesContainer")), javaObjs)
  assign("pdcFieldsDF", fieldsDF(.jnew("hec/io/PairedDataContainer")), javaObjs)
  assign("hecJavaObjectsDB", javaObjs, envir=parent.env(environment()))
  assign("DSS_CONSTANTS", J("hec/script/Constants"), envir=parent.env(environment()))
}