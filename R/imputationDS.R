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
  imp_set <- rexposome::imputation(object, select, messages = FALSE)
  return(imp_set)
}