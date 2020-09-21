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
  cmpX <- 1
  cmpY <- 2
  show.exposures <- FALSE
  
  # plot_pca <- rexposome::plotPCA(object, set = set, phenotype = phenotype)
  
  if (set == "exposures") {
    dta <- rexposome::extract(object, table = "exposures") #data.frame(object@pca$var$coord)
    dta$Family <- Biobase::pData(object@featureData)[rownames(dta), 1]
    dta$Label <- rownames(dta)
    
    if(cmpX >= ncol(dta) | cmpY >= ncol(dta)) {
      stop("Given value for 'cmpX' or 'cmpY' larger than computed ",
           "components (ncp=", ncol(dta)-1, ").")
    }
    
    plt <- ggplot2::ggplot(dta, ggplot2::aes_string(paste0("Dim.", cmpX), paste0("Dim.", cmpY))) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank()
      ) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dotdash", color = "red", size = 1) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dotdash", color = "red", size = 1) +
      ggplot2::geom_point(ggplot2::aes_string(color = "Family")) +
      ggplot2::xlab(paste0("PC", cmpX, " (", round(extract(object, table="eigen")[cmpX, 2], 2), "%)")) +
      ggplot2::ylab(paste0("PC", cmpY, " (", round(extract(object, table="eigen")[cmpY, 2], 2), "%)"))
    if(show.exposures) {
      ## Add labels for features
      plt <- plt + ggrepel::geom_text_repel(
        data = dta,
        ggplot2::aes_string(paste0("Dim.", cmpX), paste0("Dim.", cmpY), label="Label"),
        size = 2,
        box.padding = ggplot2::unit(0.35, "lines"),
        point.padding = ggplot2::unit(0.3, "lines"),
        color="#222222",
        segment.color="#BBBBBB"
      ) + ggplot2::theme_bw() + ggplot2::theme(legend.position="none")
      ## /
    }
    return(ggplot2::ggplot_build(plt))
  }
  
  return(plot_pca)
  
}