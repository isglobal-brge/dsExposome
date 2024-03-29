#' @title Scale ExposomeSet
#' 
#' @description Performs the scaling operation of the exposures centering by mean
#' and dividing by the standard deviation.
#'
#' @param Set \code{ExposomeSet} to be centered
#' @param means \code{numeric vector} that contains the means of each exposure
#' @param sds \code{numeric vector} that contains the standard deviations of each exposure
#'
#' @return \code{ExpressionSet} with the exposures scaled
#' @export


exposome_scale_exposuresDS <- function(Set, means, sds){
  
  # Extract numeric exposures from object
  select <- rownames(Biobase::fData(Set))[Biobase::fData(Set)$`.type` == "numeric"]
  exposures <- rexposome::expos(Set)[ , select]
  
  lapply(1:ncol(exposures), function(x){
    exposures[,x] - means[x]
  })

  exp_minus_mean <- sweep(exposures, 2, FUN = "-", means)
  exp_minus_mean_div_sd <- sweep(exp_minus_mean, 2, FUN = "/", sds)
  
  ans <- methods::new("ExposomeSet",
             assayData = Biobase::assayDataNew("environment", exp = t(exp_minus_mean_div_sd)),
             featureData = Biobase::featureData(Set)[select, ],
             phenoData = Biobase::phenoData(Set))
  return(ans)

}

#' @title Get means of the Exposures
#' 
#' @description Calculate the means and number of non-NA values of the exposures 
#' of an ExposomeSet.
#' 
#' @details This function has a disclosure control based on the \code{dsBase::meansDS} function. 
#'
#' @param Set \code{ExposomeSet}
#'
#' @return \code{list} that contains: \cr
#' - means \code{numeric vector} of means \cr
#' - n \code{numeric vector} of non-NA count
#' @export

exposome_scale_exposures_meansDS <- function(Set){
  
  # Extract numeric exposures from object
  select <- rownames(Biobase::fData(Set))[Biobase::fData(Set)$`.type` == "numeric"]
  exposures <- rexposome::expos(Set)[ , select]
  
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
