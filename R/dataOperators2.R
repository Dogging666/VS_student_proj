operate_on_values <- function(dataframe, operator_function, columns = NULL) {
  
  if (is.null(columns)) columns <- colnames(dataframe)
  
  result <- lapply(dataframe[, columns], function(col) sapply(col, operator_function))
  dataframe[, columns] <- as.data.frame(result)

  return(dataframe)
}

operate_on_columns <- function(dataframe, operator_function, columns = NULL) {
  # Apply the operator_function to every column in the specified range
  
  if (is.null(columns)) columns <- colnames(dataframe)
  
  result <- lapply(dataframe[, columns], operator_function)
  dataframe[, columns] <- as.data.frame(result)
  
  return(dataframe)
}

limitObservations <- function(dataframe, newObservationCount, columns) {
  
  removeOtherObservations <- function(column) {
    # Find indices of non-null values
    present_indices <- which(!is.na(column))
    
    if (length(present_indices) > newObservationCount) {
      firstNA <- present_indices[newObservationCount+1]
      
      column[firstNA:length(column)] <- NA
    }
    
    return(column)
  }
  
  return(operate_on_columns(dataframe, removeOtherObservations, columns))
}

killObservations <- function(dataframe, chance, columns) {
  
  removeObservations <- function(x) {
    # Find indices of non-null values
    if (runif(1) < chance) return(NA)
    return(x)
  }
  
  return(operate_on_values(dataframe, removeObservations, columns))
}