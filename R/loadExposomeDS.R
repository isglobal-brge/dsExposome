#' @title  Create an ExposomeSet from \code{data.frames}
#'
#' @description Takes the three tables of an exposome dataset and coerces them into a Exposome Set object
#' on the study server
#'
#' @param exposures \code{data.frame} With the exposures of the exposome
#' @param description \code{data.frame} With the description of the exposome
#' @param phenotype \code{data.frame} With the phenotype of the exposome
#' @param description.famCol (default \code{"family"}) Index where the family's
#' name (per exposures) if found in file "description". It can be both numeric
#' or character.
#' @param exposures.asFactor \code{numeric} (default \code{5}) The exposures with more
#' than this number of unique items will be considered as "continuous" while
#' the exposures with less or equal number of items will be considered as
#' "factor".
#' @param warnings \code{bool} (default \code{FALSE}) If \code{TRUE} shows useful
#'
#' @return An \code{ExposomeSet} object
#' @import rexposome
#'

loadExposomeDS <- function(exposures, description, phenotype, description.famCol = "family",
                           exposures.asFactor = 5, warnings = TRUE) {
  
  
  exposome <- rexposome::loadExposome(exposures, description, phenotype, description.famCol,
                                      exposures.asFactor, warnings)
  return(exposome)
  
  
  
}
