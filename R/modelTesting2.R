library(mgcv)
library(tidyverse)

source("R/simulator2.R")
data <- getObservedData()
seasonalitiesData <- getSeasonalities()

plot(data$days,data$contacts, pch=19, cex=0.25)

# Smooth of day only
mDay <- gam(contacts~s(days, k=100), 
         data,
         family = poisson(link = "log"), 
         method = 'REML')
summary(mDay)

gam.check(mDay, rep=500)

plot(data$days,data$contacts, pch=19, cex=0.25)

true_mean <- getTrueMean__()
points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mDay, newdata = seasonalitiesData))
points(days, pred, col="blue")

#Smooth of day + lockdown
mLockdown <- gam(contacts~s(days, k=100) + lockdown, 
         data,
         family = poisson(link = "log"), 
         method = 'REML')
summary(mLockdown)

gam.check(mLockdown, rep=500)

plot(data$days,data$contacts, pch=19, cex=0.25)

points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mLockdown, newdata = seasonalitiesData))
points(days, pred, col="blue")

#Smooth of day + lockdown + holidays
mHoliday <- gam(contacts~s(days, k=100) + lockdown + publicHoliday + schoolHoliday, 
                 data,
                 family = poisson(link = "log"), 
                 method = 'REML')
summary(mHoliday)

gam.check(mHoliday, rep=500)

plot(data$days,data$contacts, pch=19, cex=0.25)

points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mHoliday, newdata = seasonalitiesData))
points(days, pred, col="blue")

#Smooth of lockdown + holidays + day of the week!
mWeekdate <- gam(contacts~s(days, k=100) + 
                     lockdown + publicHoliday + schoolHoliday + weekdate, 
                   data,
                   family = poisson(link = "log"), 
                   method = 'REML')
summary(mWeekdate)

gam.check(mWeekdate)

plot(data$days,data$contacts, pch=19, cex=0.25)

points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mWeekdate, newdata = seasonalitiesData))
points(days, pred, col="blue")

#Smooth of everything!
mEverything <- gam(contacts~s(days, k=100) + 
                     lockdown + publicHoliday + schoolHoliday
                   + weekdate + month + year + temperature, 
                data,
                family = poisson(link = "log"), 
                method = 'REML')
summary(mEverything)

gam.check(mEverything, rep=500)

plot(data$days,data$contacts, pch=19, cex=0.25)

points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mEverything, newdata = seasonalitiesData))
points(days, pred, col="blue")

meanSquareError <- function(pred, true_mean) {
  return sum((pred-true_mean)^2) / length(true_mean)
}