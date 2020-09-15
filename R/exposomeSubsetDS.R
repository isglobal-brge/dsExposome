exposomeSubsetDS <- function(set, fam){
  
  mask <- Biobase::fData(set)$Family==fam[1]
  if (length(fam) > 1){
    for (i in 2:length(fam)) {
      indic <- Biobase::fData(set)$Family==fam[i]
      mask <- mask | indic
    }
  }

  return(set[mask,])
  
}