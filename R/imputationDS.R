#' @title Perform a imputation of an Exposome Set
#' 
#' @description Perform a imputation of an Exposome Set using rexposome base function
#'
#' @param object \code{ExposomeSet} Exposome Set object
#' @param select \code{character} \code{character} (default \code{NULL}) Name to be assigned to the exposed set on the server side. If missing,
#' the input Exposome Set will be over written.
#'
#' @return \code{ExposomeSet} Imputed Exposome Set
#'

imputationDS <- function(object, select = NULL){
  if(is.null(select)){
    imp_set <- rexposome::imputation(object, messages = FALSE)
  }
  else(
    imp_set <- rexposome::imputation(object, select)
  )
  
  return(imp_set)
}