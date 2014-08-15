#install.packages("ICE")
require(ICE) #contains the interval censored local polynomial estimators.
require(KernSmooth)

setwd("~/Dropbox/R videos/nonparametric (local polynomial) regression/")
data = read.csv("removal times.csv")
head(data)
attach(data)

############### notes ###############
#local polynomials provide a more flexible option than linear regression
#no assumptions are made about the normality of the error term

#the cost is that more data is needed to reliably estimate the model, 
#especially in multiple dimensions (multiple independent variables)
#####################################


#determine the bandwidth using a direct plug-in method (faster than cross validation)
bw = dpill(diseased.branches, removal.time) 
#estimate a local linear model
local.linear = locpoly(diseased.branches, removal.time, bandwidth=bw, degree=1)
#degree of the polynomial is not as important as the bandwidth for the regression

#plot the regression!
plot(local.linear$x, local.linear$y, type = "l", 
     xlab = "Number of diseased branch tips",ylab = "Expected removal time", lwd = 2)


################### examples of other local polynomial regressions with different parameters #################

local.linear2 = locpoly(diseased.branches, removal.time, bandwidth=bw, degree=2)
lines(local.linear2, lty = 2, lwd = 2) 

local.linear3 = locpoly(diseased.branches, removal.time, bandwidth= bw *2, degree=1)
lines(local.linear3, lty = 3, lwd = 2, col = "red") #smooths the regression (increases bias, decreases variance)

local.linear4 = locpoly(diseased.branches, removal.time, bandwidth= bw / 2, degree=1)
lines(local.linear4, lty = 4, lwd = 2, col = "blue") #decreases bias, increases variance
##############################################################################################################















################## why linear regression doesn't work here ####################

#compare this with a linear regression curve
global.linear = lm(removal.time ~ diseased.branches)
abline(global.linear, col = "green", lwd = 2) 
#taking logs of both variables doesn't help, trust me
###############################################################################


























#go back to the original plot
plot(local.linear$x, local.linear$y, type = "l", 
     xlab = "Number of diseased branch tips",ylab = "Expected removal time", lwd = 2)
















###################### making predictions and estimating marginal effects ########################
local.linear$x #x goes from 0 to 10 in 0.025 unit increments
which(local.linear$x == 0)
which(local.linear$x == 1)

#effect of diseased tips on removal time:
#0 to 1: 
local.linear$y[which(local.linear$x == 0)] - local.linear$y[which(local.linear$x == 1)]
#a tree with 1 dieased tip is expected to be removed 574.6139 days before a tree with no diseased tips

 
#0 t0 10: 
local.linear$y[which(local.linear$x == 0)] - local.linear$y[which(local.linear$x == 10)]

#3 to 6: 
local.linear$y[which(local.linear$x == 2)] - local.linear$y[which(local.linear$x == 6)]

##################################################################################################




















############################ add a line for plot average diseased tips ###########################
bw = dpill(PATDT1, removal.time) 
local.linear = locpoly(PATDT1, removal.time, bandwidth=bw, degree=1)
lines(local.linear$x, local.linear$y, lty = 2, lwd = 2) 

#add a legend
legend("topright", bty = "n", lty = 1:2, lwd = 2, legend = c("Per tree", "Plot average"))
##################################################################################################






