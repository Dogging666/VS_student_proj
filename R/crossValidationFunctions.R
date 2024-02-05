library(mgcv)
library(tidyverse)
library(tibble)
library(dplyr)

generatePartitionsEvenData <- function(data, k) {
  s <- seq(1, nrow(data))
  b <- round(seq(0, nrow(data)+1, length.out=k+1))
  c <- cut(s, breaks=b, labels = FALSE)
  return(split(s, c))
}

generatePartitionsEvenBinSize <- function(data, column, k) {
  min_val <- min(data[[column]])
  max_val <- max(data[[column]])
  
  s <- seq(1, nrow(data))
  b <- round(seq(min_val-1, max_val+1, length.out=k+1))
  c <- cut(data[[column]], breaks=b, labels=FALSE)
  return(split(s, c))
}


crossValidateGam <- function(data, partition, train, test) {
  partitions <- partition(data)
  results_vector <- numeric(0)
  
  for (partition_ in partitions) {
    trainData <- data[-partition_, ]
    test_Data <- data[ partition_, ]
    
    model <- train(trainData)
    results_vector <- c(results_vector, test(test_Data, model))
  }
  
  return(mean(results_vector))
}

# **************************************************************************** #

# Example use:

source("R/simulatorCrossValidation.R")
observationData <- getObservedData()
trueData <- getUnobservedData__()

partition <- function(data) {
  generatePartitionsEvenBinSize(data, "days", k=5)
}

trainSimple <- function(data) {
  gam(contacts~s(days, k=300),
      data,
      family = poisson(link = "log"), 
      method = 'REML'
  )
}

trainComplex <- function(data) {
  gam(contacts~s(days, k=100) + 
                lockdown + publicHoliday + schoolHoliday + weekdate, 
              data,
              family = poisson(link = "log"), 
              method = 'REML'
  )
}

test <- function(data, model) {
  old <- data[1, -which(names(data) == "contacts")]
  results <- numeric(0)
  sum <- 0; count <- 0
  pred <- predict(model, newdata=old, type="response")
  
  for (i in 1:nrow(data)) {
    new <- data[i, -which(names(data) == "contacts")]
    
    if (!identical(old, new)) {
      old <- new
      results <- c(results, sum/count)
      sum <- 0; count <- 0
      pred <- predict(model, newdata=old, type="response")
    }

    real <- data[["contacts"]][i]
    sum <- sum + (pred - real)^2
    count <- count + 1
  }
  evaluation <- sqrt(mean(results))
  return(evaluation)
}

test_true <- function(data, model) {
  minDays <- min(data["days"]); maxDays <- max(data["days"])
  data <- trueData[minDays < trueData$days & trueData$days < maxDays, ]
  
  old <- data[1, -which(names(data) == "contacts")]
  results <- numeric(0)
  sum <- 0; count <- 0
  pred <- predict(model, newdata=old, type="response")
  
  for (i in 1:nrow(data)) {
    new <- data[i, -which(names(data) == "contacts")]
    
    if (!identical(old, new)) {
      old <- new
      results <- c(results, sum/count)
      sum <- 0; count <- 0
      pred <- predict(model, newdata=old, type="response")
    }
    
    real <- data[["contacts"]][i]
    sum <- sum + (pred - real)^2
    count <- count + 1
  }
  evaluation <- sqrt(mean(results))
  return(evaluation)
}

crossValidateGam(observationData, partition, train, test)