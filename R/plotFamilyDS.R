#' @title Draw boxplot of a family of an Exposome Set
#'
#' @param x \code{ExposomeSet} Exposome Set object
#' @param family \code{character} Name of the familty that will be drawn
#' @param group \code{character} If set it displays the family grouped by the given phenotype
#' @param group2 \code{character} If set it displays the family grouped by the given phenotype
#' @param scatter \code{bool} (default \code{TRUE}) If the family to be plotted is continuous, the samples will be shown
#' @param na.omit \code{bool} (default \code{TRUE}) Do not show NA values
#'
#' @return
#' Returns a ggplot object
#' @export

plotFamilyDS <- function(x, family, group = NA, group2 = NA, scatter = TRUE, na.omit=TRUE){

  get_exposuresDS <- function(x, family, group = NA, group2 = NA, na.omit = TRUE) {
    data <- family_typeDS(x, family, as.type=TRUE)
    if (!is.na(group)) {
      if(!is.na(group2)) {
        gData <- Biobase::pData(x)[, c(group, group2), drop=FALSE]
        colnames(gData) <- c("group", "group2")
        n <- 2
      } else {
        gData <- Biobase::pData(x)[, group, drop=FALSE]
        colnames(gData) <- "group"
        n <- 1
      }
      
      data <- cbind(data[rownames(gData), ], gData)
      if(!is.na(group2)) {
        data$group <- as.factor(data$group)
        data$group2 <- as.factor(data$group2)
      } else {
        data$group <- as.factor(data$group)
      }
      
      data <- reshape2::melt(data, measure.vars = 1:(ncol(data)-n),
                             variable.factor = FALSE, rm.na = TRUE,
                             variable.name="exposures")
      
      if(na.omit) {
        if(!is.na(group2)) {
          data <- data[!is.na(data[, 1]), ]
          data <- data[!is.na(data[, 2]), ]
          data <- data[!is.na(data[, 4]), ]
        } else {
          data <- data[!is.na(data[, 1]), ]
          data <- data[!is.na(data[, 3]), ]
        }
      }
      
    } else {
      data <- reshape2::melt(data, measure.vars = 1:ncol(data),
                             variable.factor = FALSE, rm.na = TRUE,
                             variable.name="exposures")
      if(na.omit) {
        data <- data[!is.na(data[, 2]), ]
      }
    }
    return(data)
  }
  
  family_typeDS <- function(object, family, as.type = FALSE) {
    exposures <- rownames(Biobase::pData(Biobase::featureData(object)))[stringr::str_replace_all(Biobase::pData(Biobase::featureData(object))[ , 1], " ", "") == family]
    if(!as.type) {
      type <- unique(Biobase::fData(object)[exposures, ".type"] )
      if(length(type) == 1) {
        return(type)
      } else {
        return("mix")
      }
    } else {
      dd <- rexposome::expos(object)[ , exposures, drop = FALSE]
      colnames(dd) <- exposures
      rownames(dd) <- colnames(Biobase::assayData(object)[["exp"]])
      type <- Biobase::fData(object)[exposures, ".type"]
      for(ii in 1:length(type)) {
        dd[, ii] <- switch (type[ii],
                            numeric = as.numeric(dd[, ii]),
                            factor = as.factor(dd[, ii])
        )
      }
      return(dd)
    }
  }
  
  plot_exposure_numericDS <- function(x, family, group = NA, group2 = NA, scatter = TRUE, na.omit=TRUE){
    
    data <- get_exposuresDS(x, family, group, group2, na.omit)
    
    return(data)
    
  }
  
  plot_exposure_factorDS <- function(x, family, group = NA, group2 = NA, na.omit = TRUE) {
    data <- get_exposuresDS(x, family, group, group2, na.omit)
    # return(data)
    plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = "exposures", fill = "value"))
    if (!is.na(group)) {
      if(is.na(group2)) {
        plot <- plot + ggplot2::facet_wrap(~group)
      } else {
        plot <- plot + ggplot2::facet_wrap(group2~group)
      }
    }
    
    plot <- plot + ggplot2::geom_bar(position = "fill")
    plot <- plot + ggplot2::scale_y_continuous(labels = scales::percent_format())
    plot <- plot + ggplot2::ylab("Percent")
    plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
    plot <- plot + ggplot2::xlab("Exposure")

    plot
  }

  if (!family %in% stringr::str_replace_all(rexposome::familyNames(x), " ", "")) {
    stop("Given family '", family, "' not in ExposomeSet (description).")
  }
  if (!is.na(group)) {
    if (!group %in% rexposome::phenotypeNames(x)) {
      stop("Given group '", group, "' not in ExposomeSet (phenotype).")
    }
  } else {
    group <- NA
  }
  if (!is.na(group2)) {
    if (!group2 %in% rexposome::phenotypeNames(x)) {
      stop("Given group (2)'", group2, "' not in ExposomeSet (phenotype).")
    }
    if(is.na(group)) {
      group <- group2
      group2 <- NA
    }
  } else {
    group2 <- NA
  }

  typ <- family_typeDS(x, family)
  
  if (typ == "numeric") {
    
    data <- plot_exposure_numericDS(x, family, group, group2, scatter, na.omit)
    
  } else if (typ == "factor") {
    
    data <- plot_exposure_factorDS(x, family, group, group2, na.omit)
    
  } else {
    stop("Plot for mixed family is not implemented.")
  }
  
  names(data)[names(data) == "exposures"] <- "x"
  return(data)
  
}


