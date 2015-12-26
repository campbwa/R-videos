#GBM: Generalized Boosted Models
#an ensemble of classification or regression trees
#with the gbm package, you can do both a few different kinds including 
#AdaBoost and Gradient Boosting 



#advantages of GBM over logistic regression (parametric method):
  #robust to outliers
  #can still make predictions when an observation has missing data!
  #handles unequal class sizes and unbalanced predictor variables well 
  #(logistic regression isn't as good at this)
  #you don't need to specify interaction terms with tree models!
  #usually have greater predictive ability

#potential drawbacks: 
  #trees can overfit, especially if the number of 
  #ending nodes is too small or the number of trees is too large
  #definitely want to use CV, can't use in-sample 
  #prediction rate as a measure of goodness of fit!


#install.packages("gbm")
require(gbm)

# install.packages("dplyr")
#this package will change your life as it relates to data management
require(dplyr)










############# Load and transform data #################
train = read.csv("https://raw.githubusercontent.com/campbwa/R-videos/master/train.csv")
test = read.csv("https://raw.githubusercontent.com/campbwa/R-videos/master/test.csv")
head(train)
summary(train)
#missing values of age! 
#you can estimate gbm and make predictions on observations with missing values 
#in the feature space (independent variables)
#I would still recommend imputing missing values



####### Basic data manipulation ########
survived = train$Survived
train = select(train, -Survived)
end_trn = nrow(train)

#combine the two into one data set
all = rbind(train,test)
#Why? So if we manipulate variables (create new ones, cap and floor), 
#we do the same operation for the training and testing data
end = nrow(all)




#select variables to use in modeling (select is a dplyr function)
#gbm does a good job of filtering out noise variables, but will still
#get a better fit when you get rid of junk 
#(especially factor variables with lots of levels)
all = select(all
             , Pclass
             , Sex
             , Age
             , SibSp
             , Parch
             , Fare
             , Embarked
             ) 
#not many variables to choose from
#perform variable selection later




head(all)
########################################################



















########## The model #############
#as always, look at the help page for the function
?gbm

#a high guess of how many trees we'll need
ntrees = 5000


#how to tune parameters? 
#in this video, we'll tune the number of trees and 
#use reasonable values of other parameters
#test different parameters with Cross Validation 
#see the other video on this topic


Model = gbm.fit( 
  x = all[1:end_trn,] #dataframe of features
  , y = survived #dependent variable
  #two ways to fit the model
  #use gbm.fit if you are going to specify x = and y = 
  #instead of using a formula
  #if there are lots of features, I think it's easier to specify 
  #x and y instead of using a formula
  
  
  , distribution = "bernoulli"
  #use bernoulli for binary outcomes
  #other values are "gaussian" for GBM regression 
  #or "adaboost"
  
  
  , n.trees = ntrees
  #Choose this value to be large, then we will prune the
  #tree after running the model
  
  
  , shrinkage = 0.01 
  #smaller values of shrinkage typically give slightly better performance
  #the cost is that the model takes longer to run for smaller values
  
  
  , interaction.depth = 3
  #use cross validation to choose interaction depth!!
  
  
  , n.minobsinnode = 10
  #n.minobsinnode has an important effect on overfitting!
  #decreasing this parameter increases the in-sample fit, 
  #but can result in overfitting
  
  , nTrain = round(end_trn * 0.8)
  #use this so that you can select the number of trees at the end
  
  # , var.monotone = c() 
  #can help with overfitting, will smooth bumpy curves
  
  , verbose = TRUE #print the preliminary output
)  
    
  

  



#look at the last model built
#Relative influence among the variables can be used in variable selection
summary(Model)
#If you see one variable that's much more important than all of the rest,
#that could be evidence of overfitting.

#optimal number of trees based upon CV
gbm.perf(Model)

#look at the effects of each variable, does it make sense?
?plot.gbm
for(i in 1:length(Model$var.names)){
  plot(Model, i.var = i
       , ntrees = gbm.perf(Model, plot.it = FALSE) #optimal number of trees
       , type = "response" #to get fitted probabilities
       )
}

###########################################









################ Make predictions ##################
#test set predictions
TestPredictions = predict(object = Model,newdata =all[(end_trn+1):end,]
                          , n.trees = gbm.perf(Model, plot.it = FALSE)
                          , type = "response") #to output a probability
#training set predictions
TrainPredictions = predict(object = Model,newdata =all[1:end_trn,]
                           , n.trees = gbm.perf(Model, plot.it = FALSE)
                           , type = "response")


#round the predictions to zero or one
#in general, don't do this!
#it was only because the answers in the comp had to be 0 or 1
TestPredictions = round(TestPredictions)
TrainPredictions = round(TrainPredictions)
#could also mess around with different cutoff values
#would need CV to determine the best


head(TrainPredictions, n = 20)
head(survived, n = 20)



#in sample classification accuracy
1 - sum(abs(survived - TrainPredictions)) / length(TrainPredictions) 
#depending upon the tuning parameters, 
#I've gotten this as high as 99%, but that model 
#resulted in lower test set scores


#to get predicted out of sample accuracy
#need to set aside a testing data set





#write the submission
submission = data.frame(PassengerId = 1:nrow(test), survived = TestPredictions)
write.csv(submission, file = "gbm submission.csv", row.names = FALSE)
#####################################################
















#GBM vs Random Forests:
#https://www.nescent.org/wg/cart/images/9/91/Chapter6_March20.pdf
#difficult to know which method will be the best beforehand!
#"The jury is out on whether they are generally more powerful 
#than Random Forests; sometimes they are, sometimes not."
