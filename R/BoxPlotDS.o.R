#' 
#' @title Computes the statistics of the input variable required for creating a boxplot.
#' @description This function produces all the information required to plot a boxplot.
#' @details The output information is a list with 5 objects named stats, n, conf,
#' out and coef. Object stats is a 5-length vector which contains the extreme of 
#' the lower whisker (equal to the 5% quantile), the lower hinge (equal to the 25%
#' quantile), the median, the upper hinge (equal to the 75% quantile) and the
#' extreme of the upper whisker (equal to the 95% quantile). Object n is the number
#' of all non-NA observations in the dataset. Object conf gives the lower and upper
#' extremes of the notch. Object out gives the number of outliers which are below the
#' lower whisker and the number of outliers which are above the upper whisker. Object
#' coef gives the range of whiskers (how far they extend out from the box).
#' The range of whiskers is set equal to (95% quantile-75%quantile)/(interquartile range).
#' This confirms that the extreme of the lower whisker is equal to the 5% quantile
#' and the extreme of the upper whisker is equal to the 95% quantile (avoiding any 
#' possible disclosure). 
#' @param x the input (numeric) vector for which the boxplot is desired.
#' @param group the grouping (factor) vector for which the boxplot is desired.
#' @return a list which includes the objects stats, n, conf, out and coef
#' @author Avraam, D.
#'
BoxPlotDS.b <- function (x, group = NULL){

do.out = TRUE

  # Capture the nfilter for tables                       
  thr <- dsBase::listDisclosureSettingsDS()
  nf.tab <- as.numeric(thr$nfilter.tab)
  
  results_list <- list()
  
  if(!is.null(group)){
    for(level in levelsDS(group)$Levels){
      data <- x[which(group == level)]
      
      nna <- !is.na(data)
      n <- sum(nna)
      
      coef <- 1.5
      
      # stats is a vector with the extreme of the lower whisker (i.e. 5% quantile), the lower hinge (i.e. 25% quantile),
      # the median (i.e. 50% quantile), the upper hinge (i.e. 75% quantile) and the extreme of the upper whisker (i.e. 95% quantile)
      stats <- c(quantile(data, probs=1-((n-nf.tab)/n), na.rm=TRUE, names=FALSE), quantile(x, probs=0.25, na.rm=TRUE, names=FALSE),
                 quantile(data, probs=0.5, na.rm=TRUE, names=FALSE), quantile(x, probs=0.75, na.rm=TRUE, names=FALSE),
                 quantile(data, probs=((n-nf.tab)/n), na.rm=TRUE, names=FALSE))
      
      # interquantile range is the difference between 25% and 75% quantiles
      iqr <- diff(stats[c(2, 4)])
      if (coef == 0){
        do.out <- FALSE
      }else{
        upper.out <- length(which(data > (stats[4] + coef * iqr)))
        lower.out <- length(which(data < (stats[2] - coef * iqr)))
      }
      
      out <- c(lower.out, upper.out)
      
      # 95% Confidence interval of the median
      conf <- stats[3] + c(-1.58, 1.58) * iqr/sqrt(n)
      
      results_list[[length(results_list)+1]] <- list(stats, n, conf, out, coef, level)
      
    }
    
    results_list[[length(results_list)+1]] <- TRUE
    
    return(results_list)
    
  }
  
  else{
    nna <- !is.na(x)
    n <- sum(nna)
    
    #coef <- ((quantile(x, probs=1, na.rm=TRUE, names=FALSE)-quantile(x, probs=0.75, na.rm=TRUE, names=FALSE))
    #        /(quantile(x, probs=0.75, na.rm=TRUE, names=FALSE)-quantile(x, probs=0.25, na.rm=TRUE, names=FALSE)))
    
    coef <- 1.5
    
    # stats is a vector with the extreme of the lower whisker (i.e. 5% quantile), the lower hinge (i.e. 25% quantile),
    # the median (i.e. 50% quantile), the upper hinge (i.e. 75% quantile) and the extreme of the upper whisker (i.e. 95% quantile)
    stats <- c(quantile(x, probs=1-((n-nf.tab)/n), na.rm=TRUE, names=FALSE), quantile(x, probs=0.25, na.rm=TRUE, names=FALSE),
               quantile(x, probs=0.5, na.rm=TRUE, names=FALSE), quantile(x, probs=0.75, na.rm=TRUE, names=FALSE),
               quantile(x, probs=((n-nf.tab)/n), na.rm=TRUE, names=FALSE))
    
    # interquantile range is the difference between 25% and 75% quantiles
    iqr <- diff(stats[c(2, 4)])
    if (coef == 0){
      do.out <- FALSE
    }else{
      upper.out <- length(which(x > (stats[4] + coef * iqr)))
      lower.out <- length(which(x < (stats[2] - coef * iqr)))
    }
    
    out <- c(lower.out, upper.out)
    
    # 95% Confidence interval of the median
    conf <- stats[3] + c(-1.58, 1.58) * iqr/sqrt(n)
    
    list(stats, n, conf, out, coef)
  }
  
}
