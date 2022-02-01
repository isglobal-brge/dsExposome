#' @title Subset Exposome Set by familie(s)
#'
#' @param set \code{ExposomeSet} Exposome Set object
#' @param fam \code{character vector} Families to subset the exposome dataset
#'
#' @return Subsetted \code{ExposomeSet}
#' @export

exposomeSubsetDS <- function(set, fam){
  
  # The length of the subset mask will be checked using nfilter.subset
  
  #############################################################
  # CAPTURE THE nfilter SETTINGS
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.subset <- as.numeric(thr$nfilter.subset)
  #############################################################
  
  mask <- stringr::str_replace_all(Biobase::fData(set)$Family, " ", "")==fam[1]
  if (length(fam) > 1){
    for (i in 2:length(fam)) {
      indic <- stringr::str_replace_all(Biobase::fData(set)$Family, " ", "")==fam[i]
      mask <- mask | indic
    }
  }
  
  if(length(mask) <= nfilter.subset){
    stop("The resulting subset has length <= ", 
         nfilter.subset, "(DataSHIELD nfilter.subset)")
  }

  return(set[mask,])
  
}