#' @title Get family names from Exposome Set
#' 
#' @param object \code{ExposomeSet} Exposome Set object
#' @param by.exposure \code{bool} (default \code{FALSE}) If \code{TRUE} a vector labeled with each exposure 
#' name will be returned with the family of each exposures. If \code{FALSE} a vector with the 
#' (unique) name of the families of exposures will be returned.
#'
#' @return A \code{character vector} with the family names of the Exposome Set
#' @export

familyNamesDS <- function(object, by.exposure = FALSE){
  f_names <- rexposome::familyNames(object, by.exposure)
  return(f_names)
}