library(mgcv)
library(tidyverse)
library(ggplot2)

source("R/simulatorCrossValidation.R")
observationData <- getObservedData()
seasonalityData <- getSeasonality()

plot(observationData$days, observationData$contacts, type = "n"
     ,xlab = "days", ylab = "contacts", main = "cross validation example")

dataLength <- length(observationData$days)
numPartitions <- 5
partitionIndices <- round(seq(1, dataLength, length.out=numPartitions+1))
# Assumption of pre-sorted observationData here to ensure even partitions of observationData
partitions <- observationData$days[partitionIndices]

left <- partitions[2]
rght <- partitions[3]
top <- 30
btm <- -10

polygon(c(left, rght, rght, left), c(btm, btm, top, top),
        col = rgb(173, 216, 230, alpha = 128, maxColorValue = 255), border = NA)

for (partition in partitions) {
  abline(v=partition, col="red", lty=2)
}

points(observationData$days,observationData$contacts, xlab="days", ylab="contacts", pch=19, cex=0.3)



# rootMeanSquareError
# deviance = logliklhood * -2
# %devianceExpained deviance of your model vs deviance of my model
