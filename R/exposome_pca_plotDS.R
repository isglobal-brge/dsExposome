#' Title
#'
#' @param object 
#' @param set 
#' @param phenotype 
#'
#' @return
#' @export
#'
#' @examples
exposome_pca_plotDS <- function(object, set, phenotype){
  
  plot_pca <- rexposome::plotPCA(object, set = set, phenotype = phenotype)
  
  return(plot_pca)
  
}