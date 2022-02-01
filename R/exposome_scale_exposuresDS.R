#' Title
#'
#' @param Set 
#' @param means 
#'
#' @return
#' @export
#'
#' @examples
exposome_scale_exposuresDS <- function(Set, means, sds){
  
  # Extract numeric exposures from object
  select <- rownames(Biobase::fData(Set))[Biobase::fData(Set)$`.type` == "numeric"]
  exposures <- expos(Set)[ , select]
  
  lapply(1:ncol(exposures), function(x){
    exposures[,x] - means[x]
  })

  exp_minus_mean <- sweep(exposures, 2, FUN = "-", means)
  exp_minus_mean_div_sd <- sweep(exp_minus_mean, 2, FUN = "/", sds)
  
  ans <- new("ExposomeSet",
             assayData = assayDataNew("environment", exp = t(exp_minus_mean_div_sd)),
             featureData = featureData(Set)[select, ],
             phenoData = phenoData(Set))
  return(ans)

}

#' Title
#'
#' @param Set 
#'
#' @return
#' @export
#'
#' @examples
exposome_scale_exposures_meansDS <- function(Set){
  
  # Extract numeric exposures from object
  select <- rownames(Biobase::fData(Set))[Biobase::fData(Set)$`.type` == "numeric"]
  exposures <- expos(Set)[ , select]
  
  # Disclosure control adapted from dsBase::meansDS (v6.2)
  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  #nfilter.glm <- as.numeric(thr$nfilter.glm)
  #nfilter.subset <- as.numeric(thr$nfilter.subset)
  #nfilter.string <- as.numeric(thr$nfilter.string)
  #############################################################
  
  means <- lapply(exposures, function(x){
    out.numNa <- length(which(is.na(x)))
    out.totN <- length(x)
    out.validN <- out.totN-out.numNa
    
    if(out.validN < nfilter.tab){
      stop("FAILED: Nvalid less than nfilter.tab", call. = FALSE)
    }
    return(mean(x, na.rm = T))
  })
  
  ns <- lapply(exposures, function(x){
    return(sum(!is.na(x)))
  })

  return(list(means = data.frame(means), n = data.frame(ns)))
  
}
