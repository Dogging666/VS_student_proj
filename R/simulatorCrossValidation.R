library(tidyverse)
library(dplyr)

source("R/seasonalities2.R")
source("R/dataOperators2.R")
set.seed(42)

PERIOD <- 1066
p <- new.env()

days <- 1:PERIOD
p$maxPointDensity <- 10

p$seasonalityData <- generateSeasonalitiesData(days)

p$holidayAdjustment <- (
  (1 + 0.15 * p$seasonalityData$publicHoliday) *
  (1 + 0.15 * p$seasonalityData$schoolHoliday)
)

p$weekdates <- data.frame(values = p$seasonalityData$weekdate)
p$weekdates[p$weekdates == "Monday"]    <- 0.90
p$weekdates[p$weekdates == "Tuesday"]   <- 0.95
p$weekdates[p$weekdates == "Wednesday"] <- 0.95
p$weekdates[p$weekdates == "Thursday"]  <- 0.95
p$weekdates[p$weekdates == "Friday"]    <- 1.10
p$weekdates[p$weekdates == "Saturday"]  <- 1.15
p$weekdates[p$weekdates == "Sunday"]    <- 1.10
p$weekdateAdjustment <- as.numeric(p$weekdates$values)

p$monthAdjustment <- round(0.988 + 0.002 * p$seasonalityData$month, 2)

p$lockdown <- as.integer(p$seasonalityData$lockdown)
p$boundary_indices <- which(diff(p$lockdown) == -1)
p$lockdownAdjustment <- 1 - 0.7 * p$lockdown

p$sequences_to_add <- 0:6

# Modify the original vector by adding consecutive sequences
modified_vectors <- lapply(p$sequences_to_add, function(seq) p$boundary_indices + seq)

# Concatenate all modified vectors into a single vector
p$result_vector <- sort(intersect(do.call(c, modified_vectors), which(!(p$seasonalityData$lockdown))))
p$lockdownAdjustment[p$result_vector] <- 1.2

p$temperatureAdjustment <- ((p$seasonalityData$temperature - min(p$seasonalityData$temperature))
                            / (max(p$seasonalityData$temperature) - min(p$seasonalityData$temperature)))
p$temperatureAdjustment <- 0.9 + 0.2 * p$temperatureAdjustment

p$seasonalitiesAdjustment <- (
  p$holidayAdjustment * 
  p$weekdateAdjustment * 
  p$monthAdjustment * 
  p$lockdownAdjustment *
  p$temperatureAdjustment
)

p$trueMean <- p$seasonalitiesAdjustment * runif(PERIOD, min=10.0, max=12.0)

# generate draws
p$trueValues <- (sapply(p$trueMean, rpois, n = p$maxPointDensity) 
                 %>% as_tibble(names_to = NULL))

# biased removal criterion that reduces 50% of values above 20
p$reducer <- function(x) {
  if (!is.na(x) & x > 20 & runif(1) < 0.5) {
    return(x - sample(1:5, 1))
  }
  return(x)
}

weekendsAndPublicHolidays <- unique(c(
  which(p$seasonalityData$weekdate %in% c("Saturday", "Sunday")),
  which(p$seasonalityData$publicHoliday)
))

# apply removals to original observations to create new patchy observations
p$observations <- (p$trueValues 
                        %>% limitObservations(5, 1:100)
                        %>% killObservations(0.1, -weekendsAndPublicHolidays)
                        %>% killObservations(0.6,  weekendsAndPublicHolidays)
                        %>% operate_on_values(p$reducer)
)

p$unObservedData <- (data.frame(
  p$seasonalityData, t(p$trueValues)
)
  %>% pivot_longer(cols = 9:(8+p$maxPointDensity), names_to = NULL)
)
names(p$unObservedData)[9] <- "contacts"

# put together p$seasonalityData
p$observedData <- (data.frame(
  p$seasonalityData, t(p$observations)
) 
  %>% pivot_longer(cols = 9:(8+p$maxPointDensity), names_to = NULL)
)
names(p$observedData)[9] <- "contacts"
p$observedData <- p$observedData[complete.cases(p$observedData["contacts"]), ]

getSeasonality <- function() {
  return(p$seasonalityData)
}

getObservedData <- function() {
  return(p$observedData)
}

getUnobservedData__ <- function() {
  return(p$unObservedData)
}

getTrueMean__ <- function() {
  return(p$trueMean)
}