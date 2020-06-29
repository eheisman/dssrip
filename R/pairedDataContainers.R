## PairedDataContainer functions

#' getColumnsByName Get a column from a PairedDataContainer object.
#' 
#' Gets a column from a paired data container by name.
#' 
#' Name of column must be exact or NA is returned.
#'
#' @return vector of values from column.
#' @note NOTE
#' @author Evan Heisman
#' @export 
getColumnsByName <- function(file, pdc, column){
  if(class(file)=="character"){
    file = opendss(file)
  }
  if(class(pdc)=="character"){
    pdc = file$get(pdc)
  }
  if(class(column) != "character"){
    return(pdc$yOrdinates[column,])
  } else {
    if(!(column %in% pdc$labels)){
      warning(sprintf("No column named [%s] found in paired data container.", column))
      return(NA)
    }
    return(pdc$yOrdinates[which(pdc$labels == column),])
  }
}

#TODO implement this
#' columnsToDataFrame
#'
columnsToDataFrame <- function(file, pdc, colnameFunction=NULL){
  
}
