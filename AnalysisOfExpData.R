#https://github.com/campbwa/R-video-code
#data set is included



##########################################################
#####         Reading external data files             ####
##########################################################

#changing the current directory
setwd("/home/campbwa/Dropbox/R videos/intro/")
#autofill the directory by pressing "tab"


#on windows, it will look something like
#setwd("C:/users/campbwa/data")
#R is case sensitive - whether inside or outside quotes
list.files()



#I recomend using .csv files
data = read.csv("datafile.csv")
#.tsv
data = read.table("datafile.tsv", sep = "\t")
#.xlsx files
install.packages("xlsx") #this is how you install a package
#then you need to load it
require(xlsx)

#can use www.rseek.org to look up this package

#loading help
?read.xlsx



data = read.xlsx("2002-2012 Titan Variety GDD Summary.xlsx", 
                 sheetIndex = 7)
#or name the sheet
data = read.xlsx("2002-2012 Titan Variety GDD Summary.xlsx", 
                 sheetName = "Prediction -2010")

#look at the data
head(data)

#spaces in variable names are converted to .
#for example, "var 1" is converted to "var.1"



##########################################################
##########################################################























##########################################################
####           Working with a data frame              ####
##########################################################

### accessing variables from a data frame ###
data$Variety
data$GDD30.BE

### subsetting data ###
data$GDD30.BE[1:10]
#or
data[1:10,"GDD30.BE"]
#or
data[1:10,2]

### sorting data ###
sorted.data = data[order(data$GDD30.BE),]
sorted.data

### attach the data ###
attach(data)
GDD30.BE #don't have to reference the data set!
#be careful if you are working with multiple data sets

### creating subsets of data ###
levels(data$Variety)
?subset
BP = subset(data, Variety == "Blaze Prince")
CR = subset(data, Variety == "Caro Red")


#writing a file
write.csv(CR, "Caro Red.csv")

##########################################################
##########################################################
























##########################################################
####                  Simple graphics                 ####
##########################################################

#histogram
hist(data$GDD30.BE)
#change some of the defaults
hist(data$GDD30.BE, xlab = "Growing degree days", 
     ylab = "Count", main = "Histogram", col = "blue")


#scatterplot
plot(data$GDD30.BE, data$GDH30)
plot(data$GDD30.BE, data$GDH30, pch = 19, 
     col = "blue", main = "Scatter")


#different colors for different varieties
plot(BP$GDD30.BE, BP$GDH30, pch = 19, 
     col = "red", main = "Scatter",
     xlim = c(180, 520))
points(CR$GDD30.BE, CR$GDH30, pch = 19, 
     col = "blue")



##########################################################
##########################################################








################## Regression and ANOVA ####################
#linear model
?lm
BP.model = lm(GDH30 ~ GDD30.BE, data = BP)
summary(BP.model)

CR.model = lm(GDH30 ~ GDD30.BE, data = CR)
summary(CR.model)

#add the regression lines to the plot
abline(BP.model, col = "red", lwd = 2)
abline(CR.model, col = "blue", lwd = 2)


?anova
#interested in whether or not type affects growing degree days
model = lm(GDD30.BE ~ Variety, data = data)
summary(model)
anova(model)
#the answer is:
#
#NO!

############################################################





