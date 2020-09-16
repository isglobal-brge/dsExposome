#' @title Exposure Names
#' 
#' @description Extract the exposure names from a Expression Set object
#'
#' @param exposomeSet \code{ExposomeSet} Exposome Set object
#'
#' @return \code{character vector} of the exposure names
#' @import rexposome
#' @export

exposureNamesDS <- function(exposomeSet) {

  data <- rexposome::exposureNames(exposomeSet)
  return(data)

}
