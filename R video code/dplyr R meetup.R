#Three absolutely must-have packages by Hadley Wickham:
# install.packages("ggplot2")
library(ggplot2)
# install.packages("dplyr")
library(dplyr)
# install.packages("reshape2")
library(reshape2)



library(RODBC)
channel = 
sqlQuery(channel, "select * from ")


sqlSave(channel, "dbo.TableName")




#learn about dplyr and SQL server?
#caret
#shiny example
#LASSO, GBM



# train = read.csv(text = getURL("http://raw.githubusercontent.com/campbwa/R-videos/master/train.csv"))
# head(train)
#



#another way
urlfile = "https://raw.githubusercontent.com/campbwa/R-videos/master/train.csv"
train = read.csv(url(urlfile))


# train = read.csv("./Dropbox/R videos/titanic/train.csv")
head(train)


















#key functions in dplyr
#select         select columns from a dataframe
#filter         select rows from a data frame based upon criteria
#group_by       group by a factor variable
#summarize      allows you to do summary stats based upon the grouped variable
#arrange        a better way to order the data set
#left_join      identical to a sql left join, but easier (picks up the matching variables automatically)


#key functions in reshape2
#melt           rearrange the data from wide to long 

















############### selecting columns and rows of a dataframe ################

#goal: get Name, Age, and Fare in order of Age for first class passengers

######### The old way #########
#select columns from a data frame:
Step1 = train[,c("Name", "Age", "Fare", "Pclass")]

#filtering a data frame
Step2 = Step1[Step1$Pclass == 1,]

#sorting a data frame
Step3 = Step2[order(-Step2$Age),]
head(Step3, n = 10)

#have to reference the data frame all the time!
################################



















######### dplyr way #########

Step1 = select(train, Name, Age, Fare, PassengerClass = Pclass) %>% #select columns
  filter(PassengerClass == 1) %>% #only first class
  arrange(-Age)

head(Step1, n = 10)
#never have to reference the data frame after the first step!

#use %>% to pipe one command into another
#the output from the previous command becomes the input for the first argument of the
#next command
#############################





















###### Each step in more detail ######

#selecting columns
VariablesThatICareAbout = select(train, Survived, Pclass, Sex
                                 , Age, SibSp, Parch, Fare) 
head(VariablesThatICareAbout)

#everything except for PassengerId:
VariablesThatICareAbout = select(train, -PassengerId) 
head(VariablesThatICareAbout)

#columns Survived to Age
VariablesThatICareAbout = select(train, Survived:Age) 
head(VariablesThatICareAbout)


#Rename columns of the data set
VariablesThatICareAbout = select(train, PassengerClass = Pclass, Name:Age) 
head(VariablesThatICareAbout)


#selecting rows: only first class passengers
FirstClass = filter(train, Pclass == 1)
head(FirstClass)

#only first class male passengers
FirstClassMale = filter(train, Pclass == 1 & Sex == "male")
head(FirstClassMale)

#ordering the rows
train = arrange(train, Pclass, -Fare)
#######################################


##########################################################################













################## Grouping and summarizing ###############

#typically group by a factor (categorical) variable
#insanely useful for summarizing data for each factor level: counts, means, etc

#average fare by passenger class
group_by(train, Pclass) %>% #don't have to specify the dataset a second time
  summarize(AvgFare = mean(Fare), ProbSurvived = mean(Survived), N = n())
#n() is a built in function that counts the number of obs in each group


#didn't have to select those columns, group by automatically does that!
#probability of survival by gender
group_by(train, Sex) %>%
  summarize(ProbSurvived = mean(Survived), N = n())



#both categories at the same time!
#(group by two variables)
A = group_by(train, Sex, Pclass) %>% #don't have to specify the dataset a second time
  summarize(AvgFare = mean(Fare), ProbSurvived = mean(Survived), N = n())
A

#the old way was to use aggregate
A = aggregate(x = train$Fare, by = list(train$Sex, train$Pclass), FUN = "mean")
N = aggregate(x = train$Fare, by = list(train$Sex, train$Pclass), FUN = "length")
A = cbind(A, N = N$x)
#I seriously used to do this!





########### reshape ###########
# #melt comes in handy when you have two grouping variables
# A = group_by(train, Sex, Pclass, Embarked) %>% #don't have to specify the dataset a second time
#   summarize(AvgFare = mean(Fare), ProbSurvived = mean(Survived), N = n())
# A
# melt(A)
###############################





############################################################















############### Variable creation ###############
#use the mutate function
train = mutate(train
               , AgeByFare = Age * Fare
               , Age2 = Age^2
               , MaleFirstClass = ifelse(Sex == "male" & Pclass == 1, 1, 0)
               , Fare = round(Fare, 2)
               , FareAge2 = Fare * Age2
               )
head(train, n = 10)
#allows you to create variables in one step 
#without specifying the data frame a bunch of times
train$Age2 = train$Age^2

#################################################



















############## ggplot teaser ###############
#ggplot and ggvis are being covered next month in more detail
#dplyr makes it easier to produce grouped plots in ggplot

A = group_by(train, Sex, Pclass) %>% #don't have to specify the dataset a second time
  summarize(AvgFare = mean(Fare), ProbSurvived = mean(Survived), N = n())
A

ggplot(A, aes(x = Pclass, y = ProbSurvived)) + 
  geom_point(aes(size = N)) + 
  geom_line(aes(by = Sex, color = Sex), size = 1.1) + 
  ggtitle("Survival rates by gender and passenger class") + 
  xlab("Passenger class") + 
  ylab("Probability of survival")
#survival probability unaffected for male 2 or 3 class, 
#but big difference for females in 2nd and 3rd class
############################################




