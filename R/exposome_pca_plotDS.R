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
exposome_pca_plotDS <- function(object, set, phenotype, method, k, noise){
  
  if(set == "exposures"){
    plot_pca <- rexposome::plotPCA(object, set = set, phenotype = phenotype, show.exposures = TRUE, show.samples = FALSE)
    
    plot_pca <- ggplot2::ggplot_build(plot_pca)
    
    plot_pca$data[[3]]$x <- dsBase::scatterPlotDS(plot_pca$data[[3]]$x, plot_pca$data[[3]]$x, method, k, noise)[[1]]
    plot_pca$data[[3]]$y <- dsBase::scatterPlotDS(plot_pca$data[[3]]$y, plot_pca$data[[3]]$y, method, k, noise)[[1]]
    plot_pca$data[[4]]$x <- dsBase::scatterPlotDS(plot_pca$data[[4]]$x, plot_pca$data[[4]]$x, method, k, noise)[[1]]
    plot_pca$data[[4]]$y <- dsBase::scatterPlotDS(plot_pca$data[[4]]$y, plot_pca$data[[4]]$y, method, k, noise)[[1]]
    return(list(data = plot_pca$data[[3]],
                xlabel = plot_pca$plot$labels$x,
                ylabel = plot_pca$plot$labels$y,
                fams = plot_pca$plot$data$Family,
                labels = plot_pca$data[[4]]
    ))
  }
  else if(set == "samples"){
    plot_pca <- rexposome::plotPCA(object, set = set, phenotype = phenotype, show.exposures = TRUE, show.samples = FALSE)
    
    plot_pca <- ggplot2::ggplot_build(plot_pca)
    
    plot_pca$data[[3]]$x <- dsBase::scatterPlotDS(plot_pca$data[[3]]$x, plot_pca$data[[3]]$x, method, k, noise)[[1]]
    plot_pca$data[[3]]$y <- dsBase::scatterPlotDS(plot_pca$data[[3]]$y, plot_pca$data[[3]]$y, method, k, noise)[[1]]
    return(list(data = plot_pca$data[[3]],
                xlabel = plot_pca$plot$labels$x,
                ylabel = plot_pca$plot$labels$y,
                pheno = plot_pca$plot$data$phenotype
    ))
  }
  else if(set == "variance"){
    plot_pca <- rexposome:::.plot_explained(object, 1, 2)
    
    plot_pca <- ggplot2::ggplot_build(plot_pca)
    
    return(list(data = plot_pca$data[[1]],
                ylabel = plot_pca$plot$labels$y
                ))
  }
  else if(set == "variance_explained"){
    plot_pca <- rexposome:::.plot_acum (object, 1, 2)
    
    plot_pca <- ggplot2::ggplot_build(plot_pca)
    
    return(list(data = plot_pca$data[[1]],
                xline = plot_pca$data[[4]],
                yline = plot_pca$data[[3]],
                ylabel = plot_pca$plot$labels$y
    ))
  }
}