
################################################################################
#' @title Maturity offset estimation (pubertal development)
#' @description Calculate maturity offset according to the equations of Moore et al. 2015 (10.1249/MSS.0000000000000588)
#' @param data Dataset name
#' @param sex Name of the sex variable in the input dataset (0 for males and 1 for females)
#' @param age Name of the Age variable in the input dataset (in days)
#' @param height Name of the Height variable in the input dataset (in centimeters)

#' @return Estimated maturity offset according to Moore's equations.
#' @details More details can be found at Moore et al. 2015 (10.1249/MSS.0000000000000588)

#' @export  
#' @author Augusto Anguita-Ruiz & Xavier Escribá
################################################################################


maturityoffsetDS = function(data,sex,age,height){

	x <- eval(parse(text=data), envir = parent.frame())
  
	x$Maturity_offset_ <- rep(NA,nrow(x))
  
	computeMaturity_offset <- function (x) {
	  
		if (x[sex] == "0" & !is.na(x[age]) & !is.na(x[height]) ) {
	  
	  	x["Maturity_offset"] <- -7.999994 + (0.0036124 * ((as.numeric(x[age])/365)*(as.numeric(x[height])*100)))
	  
		}else if (x[sex] == "1" & !is.na(x[age]) & !is.na(x[height]) ) {
	  
		x["Maturity_offset"] <- -7.709133 + (0.0042232 * ((as.numeric(x[age])/365)*(as.numeric(x[height])*100)))
	  
	}else{}

	  return(x["Maturity_offset"])
	  
	}

	x$Maturity_offset_ <- apply(x,1,computeMaturity_offset)
  	
  	return(as.numeric(x$Maturity_offset_))
  	
}
