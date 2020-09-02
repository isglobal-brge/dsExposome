#' @title  Create an ExposomeSet from \code{data.frames}
#'
#' @description Takes the three tables of an exposome dataset and coerces them into a Exposome Set object
#' on the study server
#'
#' @param exposures \code{data.frame} With the exposures of the exposome
#' @param description \code{data.frame} With the description of the exposome
#' @param phenotype \code{data.frame} With the phenotype of the exposome
#' @param exposures.idcol \code{character} (default \code{"idcol"}) Name of the column in the Exposures file
#' that contains the individuals ID
#' @param phenotypes.idcol \code{character} (default \code{"idcol"}) Name of the column in the Phenotypes file
#' that contains the individuals ID
#' @param description.expCol \code{character} (default \code{"exposure"}) Name of the column in the Description file
#' that contains the Exposure names
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

loadExposomeDS <- function(exposures, description, phenotype, exposures.idcol = "idcol", phenotypes.idcol = "idcol",
                           description.expCol = "exposure", description.famCol = "family", 
                           exposures.asFactor = 5, warnings = FALSE) {
  
  # Exposures, assign rownames using the exposures.idcol column. Rownames of exposures files correspond to individuals ID
  exposures <- as.data.frame(exposures)
  row.names(exposures) <- unlist(exposures[, exposures.idcol])
  exposures <- exposures[ , !(names(exposures) %in% exposures.idcol)]

  # Phenotype, assign rownames using the phenotypes.idcol column. Rownames of phenotypes files correspond to individuals ID
  phenotype <- as.data.frame(phenotype)
  row.names(phenotype) <- unlist(phenotype[, phenotypes.idcol])
  phenotype <- phenotype[ , !(names(phenotype) %in% phenotypes.idcol)]
  
  # Description, assign rownames using the description.expCol column. Rownames of description files correspond to Exposure names
  description <- as.data.frame(description)
  row.names(description) <- unlist(description[, description.expCol])
  description <- description[ , !(names(description) %in% description.expCol)]
  
  exposome <- rexposome::loadExposome(exposures, description, phenotype, description.famCol,
                                      exposures.asFactor, warnings)
  return(exposome)
  
  
  
}
