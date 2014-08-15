#the three best references that I've found!
#http://cran.r-project.org/web/views/Survival.html
#http://www.ats.ucla.edu/stat/r/examples/alda/ch14.htm
#http://cran.r-project.org/doc/contrib/Fox-Companion/appendix-cox-regression.pdf


#use survival analysis when you want to know how long it takes for an "event"
#to occur:
#failure of a machine, removal of a tree, time to find employment


setwd("~/Dropbox/R videos/survival analysis/")
data = read.csv("midpoint imputed right_censored_data.csv")
attach(data)
head(data)

#the standard survival analysis package
require(survival)

























#sometimes the most difficult part: create a survival object
?Surv
head(data)
tree_surv = Surv(time, c)
head(tree_surv)



















#some descriptive statistics
#do a Kaplan Meier plot to see what the data look like
fit = survfit(tree_surv ~ 1)
plot(fit, xlab = "Days after initial inspection", 
     ylab = "Probability of survival") #K-M plot


#compare removals patterns for public and private owners
fit = survfit(tree_surv ~ public)
plot(fit, lwd = 2, lty = 1:2, xlab = "Days after initial inspection", 
     ylab = "Survival Probability")
legend("bottomleft", bty = "n", lty = 1:2, c("Private ownership", 
                                             "Public ownership"), lwd = 2)
#private owners remove trees before public owners
#crossing survival curves is evidence of nonproportional hazards


#KM curve for each type of plot
fit = survfit(tree_surv ~ as.factor(plottype))
plot(fit, lwd = 2, lty = 1:5, xlab = "Days after initial inspection", 
     ylab = "Survival Probability")
#a mess!























############ some hypothesis tests ###########
survdiff(tree_surv ~ public)
#H0 rejected!

survdiff(tree_surv ~ as.factor(plottype))
#H0 rejected!

#I think these are kind of pointless, especially with a reasonably sized data set
##############################################












#################### Weibull accelerated failure time model ###########################
#parametric survival models are estimated with survreg() function
weibull = survreg(tree_surv ~ public + TDT1 + as.factor(plottype), 
                  dist = "weibull")
summary(weibull)

#plot the hazard function associated with diseased tips
#plots and tests of the weibull model
?predict.survreg
#predicted survival time for the first tree with condfidence intervals
predict = predict(weibull, newdata = data, type='response', se=TRUE)
head(predict$fit)
head(predict$se)

















#divide the data into two sets
public.trees = subset(data, public == 1)
private.trees = subset(data, public == 0)
#predicted for public
predict.public = predict(weibull, newdata = public.trees, 
                         type='response', se.fit=TRUE)
head(predict.public$fit)
#predictions for private
predict.private = predict(weibull, newdata = private.trees, 
                          type='response', se=TRUE)
#plot!
plot(density(predict.public$fit), ylim = c(0,0.00055),
     xlab = "", main = "", lwd = 2)
lines(density(predict.private$fit), lty = 2, lwd = 2)
legend("topright", lty = 1:2, lwd = 2, legend = c("Public plots", "Private plots"), bty = "n")



















#predicted removals based on the number of diseased tips
predict.0 = predict(weibull, newdata = data[TDT1 == 0,], 
                    type='quantile', p=.5, se=TRUE)
plot(density(predict.0$fit), xlim = c(0,10000),
     ylim = c(0,0.0024), xlab = "", main = "", lwd = 2)
predict.mid = predict(weibull, newdata = data[TDT1 > 0 & TDT1 < 10,], 
                      type='quantile', p=.5, se=TRUE)
lines(density(predict.mid$fit), lty = 3, lwd = 2)
predict.ten = predict(weibull, newdata = data[TDT1 == 10,], 
                      type='quantile', p=.5, se=TRUE)
lines(density(predict.ten$fit), lty = 2, lwd = 2)
legend("topright", lty = 1:3, lwd = 2, legend = 
         c("No diseased tips", "1 to 9", "10 or more"), bty = "n")
############################################################################################



















########################### testing assumptions #######################################
#Big assumption: does the removal time follow a Weibull distribution?
uncensored = subset(data, c == 1)
head(uncensored)
plot(density(uncensored$time))
library(MASS)
fitdistr(uncensored$time, "weibull")
#shape = 2.9371517, scale = 1768.9669113
x = 0:3500
w = dweibull(x, shape = 2.9371517, scale = 1768.9669113)
lines(w, lty = 2)

#QQ plot
x = rweibull(n=nrow(uncensored),shape=2.9371517, scale=1768.9669113) 
qqplot(x, uncensored$time, xlab = "Weibull theoretical quantiles", 
       ylab = "Data quantiles", main = expression(italic(T[i] == (L[i]+R[i])/2)))
abline(0,1)
#######################################################################################




















######### Cox PH models ############
#semiparametric model, does not make assumptions about 
#the shape of the distribution of survival times
# a positive coefficient means that a tree was more likely
# to be removed earlier, given a higher value for that covariate
cox_model = coxph(tree_surv ~ public + TDT1 + as.factor(plottype) + PATDT1)
summary(cox_model)
#positive coefficients indicate higher hazard rates, more likely 
#to exit from the state




#test the proportional hazards assumption
cox.zph(cox_model)
#low p values indicate that the coefficients vary over time
plot(cox.zph(cox_model), var = "TDT1")
plot(cox.zph(cox_model), var = "public")
plot(cox.zph(cox_model), var = "PATDT1")



#####################################

















############# marginal effects #############
?predict.coxph
#Cox model is a relative risk model, does not predict time until death
predictions = predict(cox_model, newdata = data, type='risk', se.fit=TRUE)
head(predictions$fit, n = 100)
#you don't use the Cox model to get predictions on the number of days that 
#a tree survived
############################################


##### more useful
##### plots of survival functions

public.tree = data.frame(TDT1 = 8, plottype = 1, public = 1, PATDT1 = 8)
private.tree = data.frame(TDT1 = 8, plottype = 1, public = 0, PATDT1 = 8)

public.survival = survfit(cox_model, newdata = public.tree)
private.survival = survfit(cox_model, newdata = private.tree)


plot(public.survival$time, public.survival$surv, type = "l", lty = 1, 
     ylim = c(0,1),
     xlab = "Months after initial inspection", ylab = "Survival probability")
lines(private.survival$time, private.survival$surv, type = "l", lty = 2)
#now you see the proportional hazards assumption,
#the two lines will always be a proportional distance away!















