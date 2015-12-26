#Two great packages by Hadley Wickham:
# install.packages("ggplot2")
require(ggplot2)
# install.packages("dplyr")
require(dplyr)




train = read.csv("https://raw.githubusercontent.com/campbwa/R-videos/master/train.csv")
head(train)
























#key functions in dplyr
#select         select columns from a dataframe
#filter         select rows from a data frame based upon criteria
#group_by       group by a factor variable
#summarize      allows you to do summary stats based upon the grouped variable
#arrange         a better way to order the data set

















############### selecting columns and rows of a dataframe ################

#selecting columns
VariablesThatICareAbout = select(train, Survived, Pclass, Sex, Age, SibSp, Parch, Fare) 
head(VariablesThatICareAbout)

#everything except for PassengerId:
VariablesThatICareAbout = select(train, -PassengerId) 
head(VariablesThatICareAbout)

#columns Survived to Age
VariablesThatICareAbout = select(train, Survived:Age) 
head(VariablesThatICareAbout)


#selecting rows: only first class passengers
FirstClass = filter(train, Pclass == 1)

#only first class male passengers
FirstClass = filter(train, Pclass == 1 & Sex == "male")


#ordering the rows
train = arrange(train, Fare, Pclass)
##########################################################################













################## Multiple commands in a single step ###############

#the main value is the ability to pipe the output from one command into another
#use %>% to pipe one command into another


#average fare by passenger class
select(train, Pclass, Fare, Survived) %>%
  group_by(Pclass) %>% #don't have to specify the dataset a second time
  summarize(AvgFare = mean(Fare), ProbSurvived = mean(Survived), N = length(Fare))


#probability of survival by gender
select(train, Sex, Survived) %>%
  group_by(Sex) %>%
  summarize(ProbSurvived = mean(Survived))


#both categories at the same time!
#(group by two variables)
A = select(train, Pclass, Sex, Fare, Survived) %>%
  group_by(Pclass, Sex) %>% #don't have to specify the dataset a second time
  summarize(AvgFare = mean(Fare), ProbSurvived = mean(Survived), N = length(Fare))
A
#####################################################################














############### Variable creation ###############
#use the mutate function
train = mutate(train
               , AgeByFare = Age * Fare
               , Age2 = Age^2)
#allows you to create variables in one step 
#without specifying the data frame a bunch of times
train$Age2 = train$Age^2

#################################################



















############## ggplot teaser ###############
#there will be another tutorial on ggplot
#just wanted to show this to motivate using ggplot!
#dplyr makes it easier to produce grouped plots in ggplot
ggplot(A, aes(x = Pclass, y = ProbSurvived)) + 
  geom_point(aes(size = N)) + 
  geom_line(aes(by = Sex, color = Sex))
#survival probability unaffected for male 2 or 3 class, 
#but big difference for females in 2nd and 3rd class
############################################




