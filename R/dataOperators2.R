operate_on_values <- function(dataframe, operator_function) {
  # Apply the operator_function to every element in the dataframe
  result <- lapply(dataframe, function(col) sapply(col, operator_function))
  
  # Convert the result back to a dataframe
  result_dataframe <- as.data.frame(result)
  
  return(result_dataframe)
}

operate_on_columns <- function(dataframe, operator_function, start_col=1, end_col=NULL) {
  # Ensure the specified range is within the number of columns
  start_col  <- max(1, min(start_col, ncol(dataframe)))
  
  if (is.null(end_col)) end_col <- ncol(dataframe)
  else end_col <- max(1, min(end_col, ncol(dataframe)))
  
  # Apply the operator_function to every column in the specified range
  for (col_index in start_col:end_col) {
    dataframe[, col_index] <- operator_function(dataframe[, col_index])
  }
  
  return(dataframe)
}

limitObservations <- function(dataframe, newObservationCount, start_col=1, end_col=NULL) {
  
  removeOtherObservations <- function(column) {
    # Find indices of non-null values
    present_indices <- which(!is.na(column))
    
    if (length(present_indices) > newObservationCount) {
      firstNA <- present_indices[newObservationCount+1]
      column[firstNA:nrow(column),] <- NA
    }
    
    return(column)
  }
  
  return(operate_on_columns(dataframe, removeOtherObservations, start_col, end_col))
}