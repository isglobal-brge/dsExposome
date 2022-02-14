#' @title Get block SVD
#' 
#' @description Calculate block SVD decomposition
#'
#' @param Set \code{ExposomeSet}
#'
#' @return \code{data.frame} with block results
#' @export

exposome_pca_pooledDS <- function(object){
  
  dataframe <- exposures_pData(object, "exposures")
  
  # Take only numeric expositions (svd fails otherwise)
  dataframe <- dataframe[,fData(object)$`.type` == "numeric"]
  # Take expositions without missings (svd faile otherwise)
  indexes <- unlist(lapply(dataframe, function(x){
    !any(is.na(x))
  }))
  dataframe <- dataframe[,indexes]
  
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
  # Return block
  ans <- svdPartial(t(dataframe))
  # browser()
  rownames(ans) <- cls
  # colnames(ans) <- rownames(dataframe)
  return(ans)
}

#' @title Calculate partial SVD
#'
#' @param x \code{matrix}
#'
#' @return \code{matrix} with results

svdPartial <- function(x){
  
  ss <- svd(x)

  ans <- sweep(ss$u, 2, FUN="*", ss$d)
  return(ans)
  
}

#' @title Add PCA results to ExposomeSet
#' 
#' @description Adds the results of the block method to the ExposomeSet, creating a new ExposomePCA.
#'
#' @param object \code{ExposomeSet} to add the PCA results
#' @param pca \code{raw} Serialized PCA object
#'
#' @return \code{ExposomePCA}
#' @export

exposome_pca_pooled_addPCDS <- function(object, pca){
  
  # Extract numeric exposures from object
  select <- rownames(Biobase::fData(object))[Biobase::fData(object)$`.type` == "numeric"]
  exposures <- rexposome::expos(object)[ , select]
  
  # Unserialize PCA object
  pca <- unserialize(wkb::hex2raw(pca))
  
  # Correct individual princ comp values
  pca$ind$coord <- as.matrix(exposures) %*% pca$svd$V
  colnames(pca$ind$coord) <- paste0("Dim.", 1:ncol(pca$ind$coord))
  
  # Create ExposomePCA object
  class(pca) <- "list"

  ans <- new("ExposomePCA",
             assayData = Biobase::assayDataNew("environment", exp = t(exposures)),
             featureData = Biobase::featureData(object)[select, ],
             phenoData = Biobase::phenoData(object),
             pca = pca)
  return(ans)
  
}
