#' @title Add Phenotype data to ExposomeSet
#' 
#' @description Add phenotype data to an ExpressionSet
#'
#' @param x \code{ExposomeSet} ExposomeSet to which add phenotype information
#' @param exposure \code{data.frame} Table with the new exposures
#' @param identifier_ExposomeSet \code{character} Name of the ID column on the already present 
#' phenotypes data.frame on the ExposomeSet
#' @param identifier_new_exposures \code{character} Name of the ID column on the new exposure data.frame
#' @param complete_cases \code{bool} If \code{TRUE} only the matching individuals 
#' between the ExpressionSet and the phenotypes table will be included on the resulting ExpressionSet. If 
#' \code{FALSE} all the individuals on the input ExpressionSet will be on the output ExpressionSet
#'
#' @return \code{ExposomeSet} with updated phenotypes information
#' @export

addExposure2ExposomeSetDS <- function(x, exposure, identifier_ExposomeSet, identifier_new_exposures, families){
  
  if(!(any(identifier_new_exposures %in% colnames(exposure)))){
    stop("Identifier [", identifier_ExposomeSet, "] is not on the exposures table")
  }
  
  exposures_treated <- exposure %>% 
    dplyr::select(!{{identifier_new_exposures}}) %>% 
    t()
  
  valid_names <- make.names(c(rownames(x@assayData$exp), 
                              rownames(exposures_treated)), 
                            unique = T) %>% tail(n = (ncol(exposure) - 1))
  rownames(exposures_treated) <- valid_names
  
  colnames(exposures_treated) <- exposure[,identifier_new_exposures]
  
  indexes <- match(
    colnames(exposures_treated),
    colnames(x@assayData$exp)
  )
  
  x@assayData$exp <- rbind(
    x@assayData$exp, 
    exposures_treated[, indexes]
  )
  
  fData <- data.frame(
    Family = families,
    Name = rownames(exposures_treated),
    .type = lapply(exposure %>% 
                     dplyr::select(!{{identifier_new_exposures}}), function(x){
                       class(x)
                     }) %>% unlist()
  )
  rownames(fData) <- valid_names
  
  missing_cols <- colnames(x@featureData@data)[!(colnames(x@featureData@data) %in% colnames(fData))]
  
  for(i in missing_cols){
    fData[i] <- ""
  }
  
  indexes_col <- match(
    colnames(fData),
    colnames(x@featureData@data)
  )
  
  x@featureData@data <- rbind(
    x@featureData@data, 
    fData[, indexes_col]
  )
  
  return(x)
  
}