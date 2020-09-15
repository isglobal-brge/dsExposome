boxPlotGGDS <- function(data_table, group, group2){
  if (!is.null(group)) {
    if(!is.null(group2)) {
      plot_ret <- ggplot2::ggplot(data_table, ggplot2::aes_string(x = "x", y = "value", fill = "group")) +
      ggplot2::geom_boxplot() +
      ggplot2::facet_wrap(~group2)
      
    } else {
      plot_ret <- ggplot2::ggplot(data_table, ggplot2::aes_string(x = "x", y = "value", fill = "group")) +
      ggplot2::geom_boxplot()
      
    }
  } else {
    plot_ret <- ggplot2::ggplot(data_table, ggplot2::aes_string(x = "x", y = "value")) +
    ggplot2::geom_boxplot()
  }
  
  plot_ret <- plot_ret + ggplot2::scale_fill_brewer()
  plt <- ggplot2::ggplot_build(plot_ret)

  if(!is.null(group) & is.null(group2)){
    return(list(plt$data[[1]][, !names(plt$data[[1]]) %in% c("outliers")], unique(data_table$x),
                unique(data_table$group),
                counts = dplyr::count(data_table, x, group),
                "single_group"))
  }
  else if(!is.null(group) & !is.null(group2)){
    return(list(plt$data[[1]][, !names(plt$data[[1]]) %in% c("outliers")], unique(data_table$x),
                unique(data_table$group), 
                unique(data_table$group2),
                counts = dplyr::count(data_table, x, group, group2),
                "double_group"))
  }
  else{
    return(list(plt$data[[1]][, !names(plt$data[[1]]) %in% c("outliers")], unique(data_table$x),
                counts = dplyr::count(data_table, x),
                "no_group"))
  }
  
}