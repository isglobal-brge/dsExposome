normalityTestDS <- function(object, th = 0.05, min.val = 5, na.rm = TRUE,
                            warnings = TRUE){
  norm_test <- rexposome::normalityTest(object, , th, min.val, na.rm, warnings)
  return(norm_test)
}