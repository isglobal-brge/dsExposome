#' @title Compute plot parameters for ExposomeSet PCA
#' 
#' @description Computes the parameters and tables needed to be passed to the client so it can
#' render a non-disclosive plot of the PCA of an ExposomeSet
#'
#' @param object \code{ExposomeSet PCA} Exposome Set PCA object
#' @param set \code{character} Argument to specify the type of plot to compute. Options: \cr
#' 
#' -\code{"exposures"}: Plot of the exposures space on the first two principal components, color coded by family. \cr
#' -\code{"samples"}: Plot of the individuals space on the first two principal components, this plot can take the
#'  `phenotype` argument to color code the individuals by phenotypes. \cr
#' -\code{"variance"}: Plot of the variance explained by each principal component. \cr
#' -\code{"variance_explained"}: Plot of the accumulated variance explained by each principal component. \cr
#' -\code{"exposures_correlation"}: Correlation between principal components and exposures \cr
#' -\code{"phenotypes_correlation"}: Association between principal components and phenotypes \cr
#' 
#' @param phenotype \code{character} Phenotype to color code the \code{"exposures"} plot.
#' @param method \code{numeric} (1) deterministic method to anonimize the scatter plot (uses \code{k}). 
#' (2) probabilistic method to anonimize the scatter plot (uses \code{noise}).
#' @param k \code{numeric} The number of the nearest neighbors for which their centroid is calculated,
#' applied for \code{method = 1}
#' @param noise \code{numeric} The percentage of the initial variance that is used as the variance
#'  of the embedded noise.
#'
#' @return Depending on the \code{set} argument it returns: \cr
#' 
#' -\code{set == "exposures"} \cr
#'      - data: \code{data frame} raw data of the plot geometry \cr
#'      - xlabel: \code{character vector} labels of the x axis \cr
#'      - ylabel: \code{character vector} labels of the y axis \cr
#'      - fams: \code{character vector} name of the families \cr
#'      - labels: \code{character vector} name of the labels for the points (option \code{label} on the
#' client) \cr
#' 
#' -\code{set == "samples"} \cr
#'      - data: \code{data frame} raw data of the plot geometry \cr
#'      - xlabel: \code{character vector} labels of the x axis \cr
#'      - ylabel: \code{character vector} labels of the y axis \cr
#'      - pheno: \code{character vector} name of the phenotypes (option \code{phenotype} on the client) \cr
#' 
#' -\code{set == "variance"} \cr
#'      - data: \code{data frame} raw data of the plot geometry \cr
#'      - ylabel: \code{character vector} labels of the y axis \cr
#' 
#' -\code{set == "variance_explained"} \cr
#'      - data: \code{data frame} raw data of the plot geometry \cr
#'      - xline: \code{data frame} raw data of the plot x line geometry \cr
#'      - yline: \code{data frame} raw data of the plot y line geometry \cr
#'      - ylabel: \code{character vector} labels of the y axis \cr
#' 
#' -\code{set == "exposures_correlation"} \cr
#'      - data: \code{data frame} raw data of the plot geometry \cr
#' 
#' -\code{set == "phenotypes_correlation"} \cr
#'      - data: \code{data frame} raw data of the plot geometry \cr
#' 
#' @export

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
  else if(set == "exposures_correlation"){
    
    plot_pca <- rexposome::plotEXP(object)
    
    plot_pca <- ggplot2::ggplot_build(plot_pca)
    
    return(list(data = plot_pca$plot$data))
  }
  else if(set == "phenotypes_correlation"){
    plot_pca <- rexposome::plotPHE(object)
    
    plot_pca <- ggplot2::ggplot_build(plot_pca)
    
    return(list(data = plot_pca$plot$data))
  }
}