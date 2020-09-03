#' @title Draw plot with percentage of missings
#' 
#' @description Create a plot with the percentage of missings for the exposures and phenotypes of an 
#' ExposomeSet object
#'
#' @param exp \code{ExposomeSet} Exposome Set object
#' @param set \code{character} (default \code{"exposures"}) Set to get the missings plot: \code{"exposures"} or \code{"phenotypes"}
#'
#' @return
#' @export
#'
#' @examples
plotMissingsDS <- function(exp, set = "exposures"){
  plot <- rexposome::plotMissings(exp, set)
  return(plot)
}