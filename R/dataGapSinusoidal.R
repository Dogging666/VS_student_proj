library(mgcv)
library(tidyverse)

set.seed(42)
points_dense <- 10
points_light <- 0

# time index
times <- seq(1,120)

# lambda for Poisson distributed count through time
true_lambda <- round((2 + sin(times/30)) * 5, digits = 1)

# generate draws
obs <- sapply(true_lambda, rpois, n = points_dense)

if (points_light < points_dense)
for (i in (points_light+1):points_dense) {
  for (j in 41:80) {
    obs[i,j] <- NA
  }
}

# put together data
dat <- data.frame(times, t(obs)) %>% pivot_longer(cols = 2:(1 + points_dense), names_to = NULL)


# check
plot(dat$times,dat$value)

# fit model
m <- gam(value~s(times),data=dat)
summary(m)

gam.check(m)


plot(m, residuals=TRUE)
points(times,true_lambda - mean(true_lambda), cex=0.5, col="red")
# plot against data
m.pred <- predict.gam(m, newdata = data.frame(times))

plot(dat$times,dat$value, pch=19, cex=0.25)
points(times,true_lambda, pch=3, col="red")
points(times,m.pred, col = "blue")