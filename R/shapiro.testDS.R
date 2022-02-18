#' @title Shapiro-Wilk Normality Test
#' 
#' @description Perform the Shapiro-Wilk Normality Test on a numeric vector (or column of a data frame)
#'
#' @param x \code{numeric} Name of the numeric vector 
#'
#' @return Results of the Shapiro-Wilk Normality Test
#' @export
 
shapiro.testDS <- function(x){
  
  # The shapiro test can be disclosive when the object is too small
  # for that reason the nfilter.subset will be used to control the minimum
  # length of the vector (excluding NAs)
  
  #############################################################
  # CAPTURE THE nfilter SETTINGS
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.subset <- as.numeric(thr$nfilter.subset)
  #############################################################
  
  if(length(x[!is.na(x)]) <= nfilter.subset){
    stop("The vector supplied to the shapiro test has length <= ", 
         nfilter.subset, "(DataSHIELD nfilter.subset)")
  }
  
  test <- stats::shapiro.test(x)
  return(test)
  
}