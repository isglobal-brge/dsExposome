#' @title Extract ExposomeSet from imExposomeSet
#' 
#' @description Extract one of the imputed sets of an imputed ExposomeSet (imExposomeSet)
#'
#' @param x \code{imExposomeSet} to which extract one imputed ExposomeSet.
#' @param rid \code{numeric} Number of imputed set to extract.
#'
#' @return \code{ExposomeSet}
#' @export

extractExposomeSetFromImputedSetDS <- function(x, rid){
  
  return(rexposome::toES(x, rid))
  
}