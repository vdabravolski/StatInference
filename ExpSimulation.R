# R script to simulate exponential dsitribution and compare it to CLT

library("ggplot2")

rate <- 0.2 # rate parameter
n <- 40 
obs <- 1000 # number of observations
th_mean <-1/rate
th_variance <- th_mean^2
th_sd <- th_mean

#1. Show the sample mean and compare it to the theoretical mean of the distribution.
mns = NULL
for (i in 1 : obs) mns = c(mns,mean(rexp(n, rate)))
hist(mns, main="Sample means for Exponential distribution")
abline(v=th_mean,col="red",lty="dashed", lwd=4)
text(th_mean,10, "theoretical mean", col = "red")

#2. Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution.
var=NULL
for (i in 1 : obs) {
  exp <- rexp(n, rate)
  sample_mean <- mean(exp)
  sample_variance <- sum((exp - sample_mean)^2)/(n-1) # calculating the variance of each sample.
  var = c(var,sample_variance)
}
n_s <- data.frame(var)
n_s$n <- '40'


hist(var, main="Sample Variance for Exponential distribution")
abline(v=th_variance,col="red",lty="dashed", lwd=4)
text(th_variance,10, "theoretical variance", col = "red")

#3. Show that the distribution is approximately normal.

# generate variance for the dsitrbution with bigger n's.
n <- 400
var <-NULL
exp<-NULL
sample_mean <- NULL
for (i in 1 : obs) {
  exp <- rexp(n, rate)
  sample_mean <- mean(exp)
  sample_variance <- sum((exp - sample_mean)^2)/(n-1) # calculating the variance of each sample.
  var = c(var,sample_variance)
}
n_m <- data.frame(var)
n_m$n <- '400'

n <- 4000
var <- NULL
exp<-NULL
sample_mean <- NULL
for (i in 1 : obs) {
  exp <- rexp(n, rate)
  sample_mean <- mean(exp)
  sample_variance <- sum((exp - sample_mean)^2)/(n-1) # calculating the variance of each sample.
  var = c(var,sample_variance)
}
n_l <- data.frame(var)
n_l$n <- '4000'
# Combine the variance for different n's


variance_mr <- rbind(n_s,n_m,n_l)
ggplot(variance_mr, aes(var, fill = n)) + geom_density(alpha = 0.1)

