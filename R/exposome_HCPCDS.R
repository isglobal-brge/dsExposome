#' @title Hierarchical clustering on principal components
#' @description Perform a HCPC on the PCA/FAMD of an ExposomeSet
#' 
#' @details The suggested partition is the one with the higher relative loss of inertia (i(clusters n+1)/i(cluster n)).
#'
#' @param object \code{ExposomePCA} Object created by the function \link{exposome_pcaDS}
#' @param nb.clust \code{numeric} Number of clusters to find. If -1, the tree is automatically 
#' cut at the suggested level (see details). If a (positive) integer, the tree is cut with nb.cluters clusters.
#'
#' @return Object created by FactoMineR::HCPC with the original dataset removed.
#' @export
#'

exposome_HCPCDS <- function(object, nb.clust){
  if(!inherits(object, "ExposomePCA")){
    stop("Object passed to `exposome_HCPCDS` is not of class 'ExposomePCA'")
  }
  pca <- object@pca
  class(pca) <- "PCA"
  hcpc <- FactoMineR::HCPC(pca, nb.clust, graph = FALSE)
  hcpc$data.clust <- NA
  return(hcpc)
}