######
# extras.R
# Functions that would be nice to have in dssrip but aren't ready for regular use.
#
######

## Convenience function for viewing a DSS file.  Does not work without "javax/help jar"
newDSSVueWindow <- function(file=NULL){
  #javaImport("javax.help")
  mw = .jcall("hec/dssgui/ListSelection",
              returnSig="Lhec/dssgui/ListSelection;",
              method="createMainWindow")
  mw = .jnew("hec/dssgui/ListSelection")
  mw.show()
  if(!is.null(file)){
    mw.openDSSFile(file)
  }
  return(mw)
}

# used for writing timeseries data back to the dss file
#SETUP FUNCTION TO CREATE HEC TIMESERIES CONTAINER
hecTimeInt = Vectorize(function(y, m, d, hours, minutes){
  # this isn't fast, but should be reliable.
  ht = .jnew("hec/heclib/util/HecTime")
  ht$set(sprintf("%02d%03s%04d %02d%02d",as.integer(d),month.abb[m],as.integer(y),hours,minutes))
  return(ht$value())
})
