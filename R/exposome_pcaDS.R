#' Title
#'
#' @param object 
#'
#' @return
#' @export
#'
#' @examples
exposome_pcaDS <- function(object, dataset_raw){
  
  x <- dataset_raw
  
  errorMessage <- FALSE
  
  # The PCA is potentially disclosive when the calculation of the covariate matrix can be disclosive.
  # For that reason, the disclosive check of the funcion dsBase::covDS is copied, if it passes the check
  # a PCA is calculated using an off-the-shelve PCA function
  
  #############################################################
  #MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  nfilter.glm <- as.numeric(thr$nfilter.glm)
  #nfilter.subset <- as.numeric(thr$nfilter.subset)
  #nfilter.string <- as.numeric(thr$nfilter.string)
  #############################################################
  
  dataframe <- x
  
  # names of the variables
  cls <- colnames(dataframe)
  
  # number of the input variables
  N.vars <- ncol(dataframe)
  
  ######################
  # DISCLOSURE CONTROLS
  ######################
  
  ##############################################################
  # FIRST TYPE OF DISCLOSURE TRAP - TEST FOR OVERSATURATION
  # TEST AGAINST nfilter.glm								
  ##############################################################
  
  varcov.saturation.invalid <- 0
  if(N.vars > (nfilter.glm * nrow(dataframe))){
    
    varcov.saturation.invalid <- 1
    
    errorMessage <- TRUE
  }
  
  # CHECK X MATRIX VALIDITY
  # Check no dichotomous X vectors with between 1 and nfilter.tab value
  # observations at either level
  
  X.mat <- as.matrix(dataframe)
  
  Xpar.invalid <- rep(0, N.vars)
  
  for(pj in 1:N.vars){
    unique.values.noNA <- unique((X.mat[,pj])[stats::complete.cases(X.mat[,pj])])
    if(length(unique.values.noNA)==2){
      tabvar <- table(X.mat[,pj])[table(X.mat[,pj])>=1] #tabvar COUNTS N IN ALL CATEGORIES WITH AT LEAST ONE OBSERVATION
      min.category <- min(tabvar)
      if(min.category < nfilter.tab){
        Xpar.invalid[pj] <- 1
      }
    }
  }
  
  # if any of the vectors in X matrix is invalid then the function returns all the
  # outputs by replacing their values with NAs
  
  if(is.element('1', Xpar.invalid)==TRUE & varcov.saturation.invalid==0){
    
    errorMessage <- TRUE
    
  }
  
  if(errorMessage == FALSE){
    pca <- rexposome::pca(object)

    return(pca)
  }
  else{
    stop("Potentially disclosive, PCA not calculated")
  }
  
  
}