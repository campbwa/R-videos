#parallel processing assigns different parts of 
#the loop to different cores of your computer


#If you're not programming in parallel, you're
#only using part (half or less) of your computer's power!

#check on memory and temperature
#laptops tend to overheat!

#install.packages("multicore")
require(multicore)

#another way is with foreach
#http://www.r-bloggers.com/the-wonders-of-foreach/


########## timing your code ##########
#to see how long the code takes to run,
ptm <- proc.time() #put this line at the beginning of your code
proc.time() - ptm #and this line at the end
















############## #a simple example: ###############

#let's say we want to multiply a vector by 2

#first, create a vector
d = 1:4000000

#the slowest way:
#loop through each element of the vector and multiply it by 2
ptm <- proc.time()
for(i in 1:4000000){
  d[i] = d[i] * 2 
}
proc.time() - ptm

















#a faster way (vectorized processing):
#apply a transformation to a vector
#create a function for the transformation
transform = function(d){
  d = d * 2
}

#use lapply for a list or vector
#I'll cover the apply functions in another video
d = 1:4000000
ptm <- proc.time()
d = lapply(d, transform)
proc.time() - ptm

#the output is a list 
head(d)
?unlist
#unlist it!
d = unlist(d)
head(d)





















#the fastest way!
#use mclapply
?mclapply #use it just like lapply
d = 1:4000000
transform = function(d){ #don't need to re-type this
  d = d * 2
}

ptm <- proc.time()
d = mclapply(d, mc.cores = 4, transform)
proc.time() - ptm

d = unlist(d)
head(d)




















####### A bootstrap illustration #########
#adapted from
#http://climateecology.wordpress.com/2013/08/19/
#r-vs-python-speed-comparison-for-bootstrapping/
#I wrote the for loop to run in parallel


set.seed(101)
 
# generate data
x <- 0:100
y <- 2*x + rnorm(101, 0, 10)
 
# plot data
plot(x, y)
 
# run the regression
mod1 <- lm(y ~ x)
yHat <- fitted(mod1)
 
# get the residuals
errors <- resid(mod1)
 
# make a bootstrapping function
boot <- function(n = 10000){
 b1 <- numeric(n)
 
 for(i in 1:n){
 residBoot <- sample(errors, replace=F)
 yBoot <- yHat + residBoot
 modBoot <- lm(yBoot ~ x)
 b1[i] <- coef(modBoot)[2]
 }
 
 return(b1)
}
 
#Run the bootstrapping function,
#not in parallel!
system.time(bootB1 <- boot())
mean(bootB1)
















#replace the loop with a function:
boot.loop = function(i){
  residBoot <- sample(errors, replace=F)
  yBoot <- yHat + residBoot
  modBoot <- lm(yBoot ~ x)
  b1[i] <- coef(modBoot)[2]
}

#test it out with lapply first
n = 10000
b1 <- numeric(n)
bootB1 = lapply(1:10000, boot.loop)
head(bootB1)


#then use mclapply
ptm <- proc.time()
bootB1 = mclapply(1:10000, mc.cores = 4, boot.loop)
proc.time() - ptm

bootB1 = unlist(bootB1)
head(bootB1)
mean(bootB1)

#this procedure can be scaled up to as many cores as you have available!

##################################################



