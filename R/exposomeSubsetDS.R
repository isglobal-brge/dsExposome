#' @title Subset Exposome Set by familie(s)
#'
#' @param set \code{ExposomeSet} Exposome Set object
#' @param fam \code{character vector} Families to subset the exposome dataset
#'
#' @return Subsetted \code{ExposomeSet}
#' @export

exposomeSubsetDS <- function(set, fam){
  
  mask <- stringr::str_replace_all(Biobase::fData(set)$Family, " ", "")==fam[1]
  if (length(fam) > 1){
    for (i in 2:length(fam)) {
      indic <- stringr::str_replace_all(Biobase::fData(set)$Family, " ", "")==fam[i]
      mask <- mask | indic
    }
  }

  return(set[mask,])
  
}