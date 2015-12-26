setwd("/home/campbwa/Dropbox/dissertation/data")
prices = read.csv('month_avg.csv')
price = prices$Monthly_average
real = prices$Real
date = prices$Date



price = ts(price, start = 1972, frequency = 12)
real = ts(real, start = 1972, frequency = 12)


?ts



ts.plot(price, real, lwd = 2, col = c("blue", "red"))

























############# function to calculate percentage changes in data #############
pct.diff = function(price){
  PCT = rep(0,length(price))
  d = diff(price)
  for(t in 1:length(price)){
    PCT[t] = d[t] / price[t]
  }
  PCT = PCT[-length(PCT)]
  return(PCT)
}
#############################################################################


















################# Estimation of OU parameters ###################
R = pct.diff(real)
Z = 1/real[-470]
summary(lm(R ~ Z))

#the parameter mu in the OU model is close to the median of prices:
1.72750 / 0.02376 #72.70623
median(real) #72.09297


##################################################################





























############### Simulation of the OU process #######################
#this is the geometric version of the OU process
#percentage volatility is constant, absolute volatility is not constant
OU.sim <- function(T = 1000, mu = 72.70623, eta = 0.02376, sigma = 0.07419){
  P_0 = mu #starting price is the mean
  P = rep(P_0,T)
  for(i in 2:T){
    P[i] = P[i-1] + eta * (mu - P[i-1]) + sigma * rnorm(1) * P[i-1]
  }
  return(P)
}
####################################################################
















################ Plots! ##############

#default parameters
plot(OU.sim(), type = "l", xlab = "Time", ylab = "Price")

#changing the variability of the process
plot(OU.sim(sigma = 0.15), type = "l", xlab = "Time", ylab = "Price")
plot(OU.sim(sigma = 0.03), type = "l", xlab = "Time", ylab = "Price")

#changing the level of mean reversion for the process
plot(OU.sim(eta = 0.15), type = "l", xlab = "Time", ylab = "Price")
plot(OU.sim(eta = 0.001), type = "l", xlab = "Time", ylab = "Price")
#######################################


