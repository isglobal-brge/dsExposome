#' @title Add Phenotype data to ExposomeSet
#' 
#' @description Add phenotype data to an ExpressionSet
#'
#' @param x \code{ExposomeSet} ExposomeSet to which add phenotype information
#' @param pheno \code{data.frame} Table with the new phenotypes
#' @param identifier_ExposomeSet \code{character} Name of the ID column on the already present 
#' phenotypes data.frame on the ExposomeSet
#' @param identifier_new_phenotypes \code{character} Name of the ID column on the new phenotypes data.frame
#' @param complete_cases \code{bool} If \code{TRUE} only the matching individuals 
#' between the ExpressionSet and the phenotypes table will be included on the resulting ExpressionSet. If 
#' \code{FALSE} all the individuals on the input ExpressionSet will be on the output ExpressionSet
#'
#' @return \code{ExposomeSet} with updated phenotypes information
#' @export

addPhenoData2ExposomeSetDS <- function(x, pheno, identifier_ExposomeSet, identifier_new_phenotypes, complete_cases){
  
  if(!(any(identifier_ExposomeSet %in% colnames(pheno)))){
    stop("Identifier [", identifier_ExposomeSet, "] is not on the phenotypes table")
  }
  
  og_pheno <- x@phenoData@data
  og_pheno_md <- x@phenoData@varMetadata
  
  new_variables <- colnames(pheno)[!(identifier_ExposomeSet == colnames(pheno))]
  old_variables <- colnames(og_pheno)
  
  if(is.null(identifier_new_phenotypes)){
    og_individuals <- rownames(og_pheno)
  } else {
    og_individuals <- as.character(og_pheno[,identifier_new_phenotypes])
  }
  
  if(!is.null(identifier_new_phenotypes) & length(unique(og_individuals)) != nrow(og_pheno)){
    stop('The selectected identifier_new_phenotypes[', identifier_new_phenotypes, '] does not correspond to a unique identifier, there are repeated IDs in this column')
  }
  
  new_individuals <- as.character(unlist(pheno[,identifier_ExposomeSet]))
  common_individuals <- new_individuals %in% og_individuals
  
  if(all(common_individuals == FALSE)){
    stop('No common individuals between the ExpressionSet and the new pheno data')
  }
  
  new_pheno <- pheno[common_individuals,]
  new_pheno[,identifier_ExposomeSet] <- as.character(unlist(new_pheno[,identifier_ExposomeSet]))
  og_pheno <- cbind(og_pheno, og_individuals_id = og_individuals)
  og_pheno$og_individuals_id <- as.character(og_pheno$og_individuals_id)
  
  if(complete_cases == TRUE){
    rownames_new_pheno <- rownames(og_pheno[og_individuals %in% new_individuals[common_individuals],])
  } else{
    stop('complete cases FALSE not implemented')
  }
  
  if(complete_cases == TRUE){
    new_pheno <- dplyr::right_join(og_pheno, new_pheno, by = c("og_individuals_id" = identifier_ExposomeSet))
    # assay_data <- x@assayData$exp[,colnames(x@assayData$exp) %in% rownames_new_pheno]
    #Biobase::exprs(x)[,colnames(Biobase::exprs(x)) %in% rownames_new_pheno]
  }
  else{
    stop('complete cases FALSE not implemented')
    # new_pheno <- dplyr::left_join(og_pheno, new_pheno, by = c("og_individuals_id" = identifier))
    # assay_data <- Biobase::exprs(x)
  }
  
  rownames(new_pheno) <- rownames_new_pheno
  new_pheno$og_individuals_id <- NULL
  
  if(any(new_variables %in% old_variables)){stop("Variables conflict between ExposomeSet and new PhenoData")}
  for(i in new_variables){
    og_pheno_md <- eval(str2expression(paste0("rbind(og_pheno_md, ", i, " = NA)")))
  }
  
  new_pheno <- Biobase::AnnotatedDataFrame(data=new_pheno, varMetadata=og_pheno_md)
  
  x@phenoData <- new_pheno
  
  return(x)
  
}