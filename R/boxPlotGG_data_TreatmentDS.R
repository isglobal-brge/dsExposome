boxPlotGG_data_TreatmentDS <- function(table, variables, group = NULL, group2 = NULL){
  
  if(is.null(group) & !is.null(group2)){
    group <- group2
    group2 <- NULL
  }
  
  if(is.null(group2)){
    if(is.null(group)){
      data <- table[, c(variables)]
    }
    else{
      data <- table[, c(variables, group)]
    }
    
  }
  else{
    data <- table[, c(variables, group, group2)]
    # Handle case group == group2 
    if(group == group2){
      data$group2 <- data[, group]
      group2 <- "group2"
    }
  }
  # return(data)
  
  
  data <- reshape2::melt(data, measure.vars = variables, rm.na = TRUE,
                 variable.name="x")
  
  
  if(!is.null(group)){
    names(data)[names(data) == group] <- 'group'
    data[, "group"] <- as.factor(data[, "group"])
  }
  
  if(!is.null(group2)){
    names(data)[names(data) == group2] <- 'group2'
    data[, "group2"] <- as.factor(data[, "group2"])
  }
  
  
  
  
  return(data[complete.cases(data), ])
  
}