plotFamilyDS <- function(x, family, group, group2, scatter = TRUE, na.omit=TRUE){
  
  # Force scatter FALSE to prevent disclosive issues
  scatter <- FALSE
  
  get_exposuresDS <- function(x, family, group = NA, group2 = NA, na.omit = TRUE) {
    data <- family_typeDS(x, family, as.type=TRUE)
    if (!is.na(group)) {
      if(!is.na(group2)) {
        gData <- pData(x)[, c(group, group2), drop=FALSE]
        colnames(gData) <- c("group1", "group2")
        n <- 2
      } else {
        gData <- pData(x)[, group, drop=FALSE]
        colnames(gData) <- "group"
        n <- 1
      }
      
      data <- cbind(data[rownames(gData), ], gData)
      if(!is.na(group2)) {
        data$group1 <- as.factor(data$group1)
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
    exposures <- rownames(Biobase::pData(Biobase::featureData(object)))[Biobase::pData(Biobase::featureData(object))[ , 1] == family]
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
    
    # Design the plot
    if (!is.na(group)) {
      if(!is.na(group2)) {
        plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = "exposures", y = "value", fill = "group1"))
        plot <- plot + ggplot2::geom_boxplot(outlier.shape = NA)
        plot <- plot + ggplot2::facet_wrap(~group2)
      } else {
        plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = "exposures", y = "value", fill = "group"))
        plot <- plot + ggplot2::geom_boxplot(outlier.shape = NA)
        
      }
    } else {
      plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = "exposures", y = "value"))
      if(scatter) {
        plot <- plot + ggplot2::geom_point(position = ggplot2::position_jitter(width=0.3), alpha=0.1)
        plot <- plot + ggplot2::geom_boxplot(alpha=0.1, outlier.shape = NA)
      } else {
        plot <- plot + ggplot2::geom_boxplot(outlier.shape = NA)
      }
    }
    plot <- plot + ggplot2::scale_fill_brewer()
    plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
    plot <- plot + ggplot2::ylab("Measure")
    plot <- plot + ggplot2::xlab("Exposure")
    #plot <- plot + ggplot2::ggtitle(main)
    # /
    # ggplot2::ggplot_build(plot)
    plot
    
  }
  
  plot_exposure_factorDS <- function(x, family, group = NA, group2 = NA, na.omit = TRUE) {
    data <- get_exposuresDS(x, family, group, group2, na.omit)
    
    plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = "exposures", fill = "value"))
    if (!is.na(group)) {
      if(is.na(group2)) {
        plot <- plot + ggplot2::facet_wrap(~group)
      } else {
        plot <- plot + ggplot2::facet_wrap(group2~group1)
      }
    }
    
    plot <- plot + ggplot2::geom_bar(position = "fill")
    plot <- plot + ggplot2::scale_y_continuous(labels = scales::percent_format())
    plot <- plot + ggplot2::ylab("Percent")
    plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
    plot <- plot + ggplot2::xlab("Exposure")
    
    # /
    plot
  }
  
  plot_exposomeDS <- function(obj) {
    vplayout <- function(x, y) {
      grid::viewport(layout.pos.row = x, layout.pos.col = y)
    }
    
    nc <- round(sqrt(length(rexposome::familyNames(obj))))
    if(nc * nc < length(rexposome::familyNames(obj))) {
      nr <- nc + 1
    } else {
      nr <- nc
    }
    de
    ff <- rexposome::familyNames(obj)
    
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nr, nc)))
    
    idx <- 1
    for(ii in 1:nr) {
      for(jj in 1:nc) {
        if(idx < length(ff) + 1) {
          typ <- family_typeDS(obj, ff[idx])
          if (typ == "numeric") {
            plt <- plot_exposure_numericDS(obj, family = ff[idx], scatter = FALSE)
          } else if (typ == "factor") {
            plt <- plot_exposure_factorDS(obj, family = ff[idx])
          } else {
            stop("Plot for mixed family is not implemented.")
          }
          plt <- plt + ggplot2::ggtitle(ff[idx])
          if(jj != 1) {
            plt <- plt + ggplot2::ylab("")
          }
          if(ii != nr) {
            plt <- plt + ggplot2::xlab("")
          }
          print(plt, vp = vplayout(ii, jj))
          idx <- idx + 1
        }
      }
    }
    
  }

  # If family is 'all' all the exposome is shown
  if(tolower(family) == "all") {
    
    return(plot_exposomeDS(x))
  }
  # /

  if (!family %in% rexposome::familyNames(x)) {
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
    plt <- plot_exposure_numericDS(x, family, group, group2, scatter, na.omit)
    print(plt)
    return(invisible())
  } else if (typ == "factor") {
    plt <- plot_exposure_factorDS(x, family, group, group2, na.omit)
    print(plt)
    return(invisible())
  } else {
    stop("Plot for mixed family is not implemented.")
  }

}


