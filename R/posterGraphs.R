source("R/simulatorCrossValidation.R")

library(ggplot2)
library(tidyverse)
library(mgcv)

convertDay <- function(day) {
  return(as.Date("2020-01-01") + day - 1)
}

# means Data -------------------------------------------------------------------
meansData <- data.frame(
  days = convertDay(days),
  mean = getTrueMean__()
)

xMid <- convertDay(100)
yMax <- 24.33
yMin <-  2.41

lockdownStart <- c(
  as.Date("2020-03-30"), 
  as.Date("2020-07-08"),
  as.Date("2021-02-12"),
  as.Date("2021-05-27"),
  as.Date("2021-07-15"),
  as.Date("2021-08-05")
)

lockdownEnd <- c(
  as.Date("2020-05-12"), 
  as.Date("2020-10-27"),
  as.Date("2021-02-17"),
  as.Date("2021-06-10"),
  as.Date("2021-07-27"),
  as.Date("2021-10-21")
)

shaded_regions = data.frame(lockdownStart, lockdownEnd)

(
ggplot(meansData, aes(x = days, y = mean))
+ geom_point(alpha = 0.6)
+ labs(title = "Expected close contacts over time", x="Days", y="Expected contacts")
+ expand_limits(y = 0)
+ theme(plot.title = element_text(hjust=0.5, size=16, face="bold"))
  
  # Add lockdown periods
+ geom_rect(data = shaded_regions, inherit.aes=FALSE,
  aes(xmin = lockdownStart, xmax = lockdownEnd, ymin=-Inf, ymax=+Inf), 
  alpha=0.2)
  
  # Add horizontal lines
+ geom_hline(yintercept = yMax, linetype="dashed", color="blue")
+ geom_hline(yintercept = yMin, linetype="dashed", color="red")
+ geom_hline(yintercept = 0)
+ geom_vline(xintercept = convertDay(0))
  
+ geom_label(aes(x = xMid, y = yMax+1, label = "Max Possible"),
             fill = "lightgrey", colour = "blue",
             label.size = NA, size = 4)
  
+ geom_label(aes(x = xMid, y = yMin-1, label = "Min Possible"),
             fill = "lightgrey", colour = "red",
             label.size = NA, size = 4)
)


# Modifications due to close contacts ------------------------------------------

trueData <- getUnobservedData__()
trueSketch <- distinct(trueData[trueData$days %in% 0:100,])

observedData <- getObservedData()
observedSketch <- distinct(observedData[observedData$days  %in% 0:100,])

observedTrue <- inner_join(trueSketch, observedSketch, by=c("days", "contacts"))
observedNotTrue <- anti_join(observedSketch, trueSketch, by=c("days", "contacts"))
notObservedTrue <- anti_join(trueSketch, observedSketch, by=c("days", "contacts"))

(
  ggplot()
  + geom_point(data = notObservedTrue, aes(x = days, y = contacts), size=2, alpha = 1.0, color="black", shape=1)
  + geom_point(data = observedTrue,    aes(x = days, y = contacts), size=2, alpha = 1.0, color="black", shape=16)
  + geom_point(data = observedNotTrue, aes(x = days, y = contacts), size=2, alpha = 1.0, color="red", shape=16)
  + labs(title = "Impact of modification on data", x="Days", y="Close contacts")
  + expand_limits(y = 0)
  + theme(plot.title = element_text(hjust=0.5, size=16, face="bold"))
)

# Cross Validation ------------------------------------------------------------
observedData <- getObservedData()

k<-5
sequence <- round(seq(min(observedData$days)-1, max(observedData$days)+1, length.out=k+1))
toTest <- sequence[2]:sequence[3]

trainingData <- observedData[!(observedData$days %in% toTest),]
simpleModel <- gam(contacts~s(days, k=300)
    , data = trainingData
    , family = poisson(link = "log")
    , method = 'REML'
)
complexModel <- gam(contacts~s(days, k=100)
      + lockdown + publicHoliday + schoolHoliday + weekdate 
                   , data = trainingData
                   , family = poisson(link = "log")
                   , method = 'REML'
)

trainingData$days <- convertDay(trainingData$days)
sequence <- convertDay(sequence)
left <- sequence[2]; rght <- sequence[3]

predData1 <- data.frame(days=convertDay(observedData$days), 
                       contacts=predict(simpleModel,  newdata=observedData, type="response"))

predData2 <- data.frame(days=convertDay(observedData$days), 
                        contacts=predict(complexModel, newdata=observedData, type="response"))

(
  ggplot()
  + geom_rect(data=data.frame(left, rght),
              aes(xmin = left, xmax = rght, ymin=-Inf, ymax=+Inf), alpha=0.1)
  + geom_point(data = distinct(trainingData), aes(x = days, y = contacts), color="grey50", size=0.8, shape=1)
  + geom_point(data = distinct(testSketch), aes(x = days, y = contacts), color="grey50", size=0.8, shape=1)
  + geom_point(data = predData2, aes(x = days, y = contacts), color="blue", size=1.2, shape=1)
  + geom_point(data = predData1, aes(x = days, y = contacts), color="red",  size=1.2, shape=1)
  + labs(title = "Cross-validation", x="Days", y="Close contacts")
  + expand_limits(y = 0)
  + theme(plot.title = element_text(hjust=0.5, size=16, face="bold"))
  + geom_vline(xintercept=sequence, linetype="dashed", color="black")
  + geom_hline(yintercept = 0)
  + geom_vline(xintercept = convertDay(0))
)