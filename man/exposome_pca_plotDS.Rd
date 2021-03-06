% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exposome_pca_plotDS.R
\name{exposome_pca_plotDS}
\alias{exposome_pca_plotDS}
\title{Compute plot parameters for ExposomeSet PCA}
\usage{
exposome_pca_plotDS(object, set, phenotype, method, k, noise)
}
\arguments{
\item{object}{\code{ExposomeSet PCA} Exposome Set PCA object}

\item{set}{\code{character} Argument to specify the type of plot to compute. Options: \cr

-\code{"exposures"}: Plot of the exposures space on the first two principal components, color coded by family. \cr
-\code{"samples"}: Plot of the individuals space on the first two principal components, this plot can take the
 `phenotype` argument to color code the individuals by phenotypes. \cr
-\code{"variance"}: Plot of the variance explained by each principal component. \cr
-\code{"variance_explained"}: Plot of the accumulated variance explained by each principal component. \cr
-\code{"exposures_correlation"}: Correlation between principal components and exposures \cr
-\code{"phenotypes_correlation"}: Association between principal components and phenotypes \cr}

\item{phenotype}{\code{character} Phenotype to color code the \code{"exposures"} plot.}

\item{method}{\code{numeric} (1) deterministic method to anonimize the scatter plot (uses \code{k}). 
(2) probabilistic method to anonimize the scatter plot (uses \code{noise}).}

\item{k}{\code{numeric} The number of the nearest neighbors for which their centroid is calculated,
applied for \code{method = 1}}

\item{noise}{\code{numeric} The percentage of the initial variance that is used as the variance
of the embedded noise.}
}
\value{
Depending on the \code{set} argument it returns: \cr

-\code{set == "exposures"} \cr
     - data: \code{data frame} raw data of the plot geometry \cr
     - xlabel: \code{character vector} labels of the x axis \cr
     - ylabel: \code{character vector} labels of the y axis \cr
     - fams: \code{character vector} name of the families \cr
     - labels: \code{character vector} name of the labels for the points (option \code{label} on the
client) \cr

-\code{set == "samples"} \cr
     - data: \code{data frame} raw data of the plot geometry \cr
     - xlabel: \code{character vector} labels of the x axis \cr
     - ylabel: \code{character vector} labels of the y axis \cr
     - pheno: \code{character vector} name of the phenotypes (option \code{phenotype} on the client) \cr

-\code{set == "variance"} \cr
     - data: \code{data frame} raw data of the plot geometry \cr
     - ylabel: \code{character vector} labels of the y axis \cr

-\code{set == "variance_explained"} \cr
     - data: \code{data frame} raw data of the plot geometry \cr
     - xline: \code{data frame} raw data of the plot x line geometry \cr
     - yline: \code{data frame} raw data of the plot y line geometry \cr
     - ylabel: \code{character vector} labels of the y axis \cr

-\code{set == "exposures_correlation"} \cr
     - data: \code{data frame} raw data of the plot geometry \cr

-\code{set == "phenotypes_correlation"} \cr
     - data: \code{data frame} raw data of the plot geometry \cr
}
\description{
Computes the parameters and tables needed to be passed to the client so it can
render a non-disclosive plot of the PCA of an ExposomeSet
}
