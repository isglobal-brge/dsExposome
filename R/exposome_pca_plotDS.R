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
  
  return(ggplot2::ggplot_build(plot_pca))
  
}