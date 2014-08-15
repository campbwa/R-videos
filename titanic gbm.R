#GBM: Generalized Boosted Models
#an ensemble of classification or regression trees
#with the gbm package, you can do both a few different kinds including 
#AdaBoost and Gradient Boosting 


#GBM vs Random Forests:
#https://www.nescent.org/wg/cart/images/9/91/Chapter6_March20.pdf
#difficult to know which method will be the best beforehand!
#"The jury is out on whether they are generally more powerful 
#than Random Forests; sometimes they are, sometimes not."



#advantages of tree methods over logistic regression:
  #robust to outliers
  #can still make predictions when an observation has missing data!
  #can handle unequal class sizes (logistic regression isn't as good at this)

#disadvantages: 
  #trees can seriously overfit, especially if the number of 
  #ending nodes is too small
  #definitely want to use CV, can't use in-sample 
  #prediction rate as a measure of goodness of fit!


#install.packages("gbm")
require(gbm)











############# Load and transform data #################
setwd("/home/campbwa/Dropbox/kaggle competitions/titanic/")

train = read.csv("https://raw.githubusercontent.com/campbwa/R-videos/master/train.csv")


#I filled in missing ages using a linear model 
#(see the logistic regression video)
train = read.csv("train data with estimated age.csv")
head(train)
survived = train$survived
train = train[,-1]
end_trn = nrow(train)


test = read.csv("test with estimated age.csv")
head(test)
#remove the ID column
test = test[,-1]

#combine the two into one data set
train = rbind(train,test)
end = nrow(train)

#strategically remove variables
#train = train[, c(-2)] 
train = train[, c(-2, -7, -9, -10, -11)] 
#although these variables help with in-sample prediction,
#including them reduces the out of sample predictive ability!

head(train)
#you don't need to specify interaction terms with tree models!
########################################################



















########## The model #############
#as always, look at the help page for the function
?gbm

#store predictions for test and train sets in these variables
pr = 0
tr = 0
n.models = 5 #the number of models to run
#the end prediction will be the average over these models
ntrees = 2000

#how to tune parameters? 
#There are more parameters to choose than with a Random Forest
#test different parameters with Cross Validation 
#see the other video on this topic



for (i in 1:n.models){
  GBM.model = gbm.fit( 

    x = train[1:end_trn,]
    , y = survived
    #two ways to fit the model
    #use gbm.fit if you are going to specify x = and y = 
    #instead of using a formula
    #if there are lots of features, it's easier to specify 
    #x and y instead of using a formula
    
    
    , distribution = "bernoulli"
    #use bernoulli for binary outcomes
    #other values are "gaussian" for GBM regression 
    #or "adaboost"
    
    
    , n.trees = ntrees
    #Choose this value to be large, then we will prune the
    #tree after running the model
    
    
    , shrinkage = 0.001 
    #smaller values of shrinkage (al-
    #most) always give improved predictive performance.
    #the cost is that the model takes longer to run for smaller values
    
    
    , interaction.depth = 5
    #use cross validation to choose interaction depth!!
    
    
    , n.minobsinnode = 5
    #n.minobsinnode has an important effect on overfitting!
    #if you set it equal to 1, it would probably perfectly 
    #fit all observations in the sample
    #decreasing this parameter increases the in-sample fit, 
    #but results in overfitting
    
#     , var.monotone = c()    
    , verbose = TRUE) #print the preliminary output
    
  
  #test set predictions
  pr1 = predict(object = GBM.model,newdata =train[(end_trn+1):end,], ntrees)
  #training set predictions
  tr1 = predict(object = GBM.model,newdata =train[1:end_trn,], ntrees)
  
  pr = pr+pr1
  tr = tr+tr1
}

#GBM is a random model - it takes random subsamples of data and features
#average the predictions from each model to reduce some of the variability
pr = pr/n.models
tr = tr/n.models
head(pr)
head(tr)

summary(GBM.model)
#If you see one variable that's much more important than all of the rest,
#that could be evidence of overfitting.


#look at the effects of each variable, does it make sense?

###########################################













#round the predictions to zero or one
pr = round(pr)
tr = round(tr)
#could also mess around with different cutoff values
#would need CV to determine the best


head(tr, n = 20)
head(survived, n = 20)



#in sample classification accuracy
1 - sum(abs(survived - tr)) / nrow(train) 
#depending upon the tuning parameters, 
#I've gotten this as high as 99%, but that model 
#resulted in lower test set scores




#write the submission
submission = data.frame(PassengerId = 1:nrow(test), survived = pr)
write.csv(submission, file = "gbm submission.csv", row.names = FALSE)










