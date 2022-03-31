#' @title Anderson-Darling Normality Test
#' 
#' @description Perform the Anderson-Darling Normality Test on a numeric vector (or column of a data frame)
#'
#' @param x \code{numeric} Name of the numeric vector 
#'
#' @return Results of the Anderson-Darling Normality Test
#' @export

anderson.darling.testDS <- function(x){
  
  # The k-s test can be disclosive when the object is too small
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
  test <- nortest::ad.test(x)
  
  return(test)
  
}