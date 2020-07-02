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
