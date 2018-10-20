# Author: Anani Assoutovi
# @Tag: יְיָ אֱלֹהֵֽינוּ וֵֽאלֹהֵי אֲבוֹתֵֽינוּ
# Date: October 13th 2018
# Time: 14:12:12

# Clean Environment:
rm(list = ls())

# sFunction: installing and loading of packages
install_load <- function (packages)  {   
  
  # Start loop to determine if each package is installed
  for(package in packages){
    
    # If package is installed locally, load
    if(package %in% rownames(installed.packages()))
      do.call('library', list(package))
    
    # If package is not installed locally, download, then load
    else {
      install.packages(package, dependencies = TRUE)
      do.call("library", list(package))
    }
  } 
}
# Generic libraries loading
libs <- c("ggplot2", "dplyr", "scales", "ggthemes", "mice", "randomForest")
install_load(libs)

# Getting the OS of your machine so we can:
  # A: Set the Working Directory of your R Script
  # B: Get the Working Directory of your R Script
# These two function are not mandated, I do this because I work from multiple machines with different
  # Directory on each machine. The setWorkDir() can be change to be a little modular
  # Bare with Me.
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else {
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  return(tolower(os))
}
setWorkDir <- function(){
  if(get_os() == "windows"){
    setWork <- setwd("C:/Users/aassoutovi/Documents/RStudioFiles/CaseStudies")
  }
  else if(get_os() == "osx"){
    setWork <- setwd("~/Desktop/MachineLearningVisualized_R/")
  }
  else{
    print("I DO NOT RECOGNIZED YOUR OS!!!")
  }
}
setWorkDir()
getwd()

#Now we import the dataset
#store the main dataset
#Divide the dataset into training and testing features
# To my desire, I am splitting the data 80/20 (80% for training and 20% for testing)
#particularly in this case, we recieved 2 different data, a training set and a testing set
#I am not too sure how effective does that are so I am doing my own splitting

#testPathRead <- read.csv(file.choose(), header =T, sep =",")
train <- read.csv("~/Desktop/PrinOfDataScience/titanic/input/train.csv", head=TRUE,sep=",")
test <- read.csv("~/Desktop/PrinOfDataScience/titanic/input/test.csv", head=TRUE,sep=",")
data <- bind_rows(train, test)


# Long explaination short, set.seed() give your data consistency
set.seed(33)

# First thing we do with Data once received, we analyze it.
# Some popular quick analysis methods are str(), summary(), head(), tail()
str(data)
summary(data)
head(data)
tail(data)

#Because we are trying to be efficient in finding quick results, 
  #Therefore, let us Create a new column called Ref which is an indication to the Passenger's title by gender
  #use a little regex to get each passenger's reference title and then we can redesign the titles
data$Ref <- gsub('(.*, )|(\\..*)', '', data$Name)

#Now that we have each passenger's Reference, let us show the Reference by Sex
  # Declaring an array for the References
unknown_ref <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
table(data$Sex, data$Ref)
data$Ref[data$Ref == 'Mlle'] <- 'Miss'
data$Ref[data$Ref == 'Ms'] <- 'Miss'
data$Ref[data$Ref == 'Mme'] <- 'Mrs'
data$Ref[data$Ref %in% unknown_ref] <- 'Unknown'

# Nevermind the method below, it needs some tweaking to be fully functional
updateRef <- function(parData, colName, oldRef, newRef){
  return(parData$colName[parData$colName == oldRef] <- newRef)
}

table(data$Sex, data$Ref)
data$FamSize <- data$SibSp + data$Parch + 1
data$Family <- paste(data$Surname, data$FamSize, sep ='')


# Use ggplot2 to visualize the relationship between family size & survival
ggplot(data, aes(x = FamSize, fill = factor(Survived))) +
  geom_bar(stat='count', position='stack') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  labs(y = "Count") +
  theme_few()

data$FamSizeRef[data$FamSize == 1] <- 'singleton'
data$FamSizeRef[data$FamSize <= 4 & data$FamSize >= 1] <- 'small'
data$FamSizeRef[data$FamSize >= 5] <- 'large'
mosaicplot(table(data$FamSizeRef, data$Survived), main ='The Size of Family Survived', shade =T)

# Now we need to strip the NULL values from the cabin
strsplit(data$Cabin[2], NULL)[1]

# Now we can get rid of our missing passenger IDs
free_fare <- data %>% filter(PassengerId != "")
#Now, Let us plot
ggplot(free_fare, aes(x= Embarked, y =Fare, fill =factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), col ='orange', linetype ='dotted', lwd =2) +
  scale_y_continuous(labels =dollar_format()) +
  theme_few()

data$Embarked[c(62, 830)] <- 'C'

# We can now plot to see the fares of all others sharing their classes
ggplot(data[data$Pclass == '3' & data$Embarked == 'S', ],aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)), col ='orange', linetype='dotted', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()

# Now, we can replace missing fares value with median far
data$Fare <- median(data[data$Pclass == '3' & data$Embarked == 'S', ]$Fare, na.rm = TRUE)

# Now we can show the numbers of missing values again
sum(is.na(data$Age))

factors_scope <- c('PassengerId', 'Pclass', 'Sex', 'Embarked', 'Ref', 'FamSizeRef', 'Family', 'FamSize')
data[factors_scope] <- lapply(data[factors_scope], function(x) as.factor(x))

# Using Mice for imputation
using_mice <- mice(data[, !names(data) %in% c('PassengerId','Name','Ticket','Cabin','Family','FamSizeRef','Survived')], method = 'rf')

mice_output <- complete(using_mice)
#Now, let's plot something
par(mfrow =c(1,2))
hist(data$Age, freq =F, main ='Age: Original Data', col ='light blue', ylim =c(0, 0.04))
hist(mice_output$Age, freq =F, main ='Age: MICE Output', col ='turquoise', ylim =c(0, 0.4))

# Now we can replace the Age Variable with Mice Model
data$Age <- mice_output$Age
sum(is.na(data$Age))

# Let us plot this now
ggplot(data, aes(Age, fill =factor(Survived))) +
  geom_histogram() +
  facet_grid(.~Sex) +
  theme_few()

# Now, we can create a column to determine if Child or Adult
data$Child[data$Age < 18] <- 'Child'
data$Child[data$Age >= 18] <- 'Adult'
table(data$Child)

# Let us do the same for Mothers or not
data$Mother <- 'Not Mother'
data$Mother[data$Sex == 'female' & data$Parch > 0 & data$Age > 18 & data$Ref != 'Miss'] <- 'Mother'
table(data$Mother, data$Survived)


data$Child <- factor(data$Child)
data$Mother <- factor(data$Mother)
md.pattern(data)


#FYI when you bind your variable with (), it print out the variable
titanic.Ind <- sample(2, nrow(data),replace =T, prob =c(0.8,0.2))
titanic.Train <- data[titanic.Ind==1,]
titanic.Test <- data[titanic.Ind==2,]

set.seed(101)

#new_Model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Embarked + Ref + Fare + FamSize + Child + Mother, data =data)












