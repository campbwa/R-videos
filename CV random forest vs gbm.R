#If you don't cross validate, you are not obtaining a realistic measure
#of your models predictive accuracy!
#Anyone can make a model that fits the data perfectly, but has terrible 
#out of sample accuracy. (overfitting)

#at least two types of classification error can be used:
#1. area under the ROC curve (AUC)
#2. missclassification rate

#A reliable and valid AUC estimate can be interpreted as the 
#probability that the classifier will assign 
#a higher score to a randomly chosen positive 
#example than to a randomly chosen negative example.



#load the package to determine AUC:
require(verification)
?roc.area

#packages for the models that I'm going to use
require(randomForest)
require(gbm)













setwd("~/Old Kaggle comps/amazon")
train = read.csv("train.csv")
head(train)


#for unbalanced datasets, the "most useful" measure is AUC
mean(train$ACTION) #94% of ACTION is equal to 1



















#################### partition the data #####################
#there's a function in plyr that will do this, but it's easy to do your own
#for k-fold CV, you create k different partitions in the data
#I'm assuming that my data are already in a random order

k = 10
n = floor(nrow(train)/k) #n is the size of each fold
#I rounded down to avoid going out of bounds on the last fold
err.vect = rep(NA,k) #store the error in this vector

#how to partition the first fold
i = 1
s1 = ((i - 1) * n+1) #the start of the subset
s2 = (i * n)       #the end of the subset
subset = s1:s2   #the range of the subset 
#because of rounding, the end of the subset may be slighly out of range
  
cv.train = train[-subset,] #train the model using this data    
cv.test = train[subset,] #test the model's performance on this data

#to do "standard" CV, we could just run the model on the cv.train data
#and test it on the cv.test data
#k-fold CV allows us to use all of the data for the final model
#but still have realistic model performance estimates 

#next, move to the second fold:
i = 2
#...
##############################################################








########################### CV for random forest ############################
#need to loop over each of the folds
for(i in 1:k){
  s1 = ((i - 1) * n+1) #the start of the subset
  s2 = (i * n)       #the end of the subset
  subset = s1:s2   #the range of the subset 
  
  cv.train = train[-subset,] #train the model using this data    
  cv.test = train[subset,] #test the model's performance on this data
  
  #run the random forest on the train set
  fit = randomForest(x = cv.train[,-1], y = as.factor(cv.train[,1]))
  #make predictions on the test set
  prediction = predict(fit, newdata = cv.test[,-1], type = "prob")[,2]
  
  #calculate the model's accuracy for the ith fold
  err.vect[i] = roc.area(cv.test[,1], prediction)$A 
  print(paste("AUC for fold", i, ":", err.vect[i]))
}
print(paste("Average AUC:", mean(err.vect)))

#each fold has a different error rate,
#and that's why you do k-fold CV!

##############################################################################













########################### CV for gbm ############################
ntrees = 1000 #the default is only 100
for(i in 1:k){
  s1 = ((i - 1) * n+1) #the start of the subset
  s2 = (i * n)       #the end of the subset
  subset = s1:s2   #the range of the subset 
  
  cv.train = train[-subset,]    
  cv.test = train[subset,] #test the model's performance on this data
  
  #estimate the gbm on the cv.train set
  fit = gbm.fit(x = cv.train[,-1], y = cv.train[,1], 
                n.trees = ntrees, verbose = FALSE, shrinkage = 0.005, 
                interaction.depth = 20, n.minobsinnode = 5, distribution = "bernoulli") 
  #use bernoulli or adaboost for classification problems
  #make predictions on the test set
  prediction = predict(fit, newdata = cv.test[,-1], n.trees = ntrees)
  err.vect[i] = roc.area(cv.test[,1], prediction)$A 
  print(paste("AUC for fold", i, ":", err.vect[i]))
}
print(paste("Average AUC:", mean(err.vect)))

#conclusion: a random forest is better for this data set! 
#(and for these parameters)
#only needed to change the model fit and prediction lines of the code!
##################################################################


















################ What's next? ###################

#you can use this method to optimize the parameters of each model
#GBM has several parameters that must be specified
#compare more types of models
#a better measure of your model's out of sample accuracy 

#################################################






