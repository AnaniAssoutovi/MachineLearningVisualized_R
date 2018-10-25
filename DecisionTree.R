# Author: Anani Assoutovi
# @Tag: יְיָ אֱלֹהֵֽינוּ וֵֽאלֹהֵי אֲבוֹתֵֽינוּ
# Date: October 25th 2018
# Time: 16:12:12

# Cleaning The Environment
rm(list =ls())

# This Function: Installing and loading of packages
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
libs <- c("ISLR", "randomForest", "tree", "ggplot2")
install_load(libs)

getwd()

# We need to load the ISLR package that includes the 'Carseats' data that we are going to use
data(package="ISLR")

# Here, we are storing the Carseats data into a variable names carSeats
carSeats <- Carseats

summary(carSeats)
head(carSeats)
tail(carSeats)
str(carSeats)

# Histogram of the car sales in dataset
hist(carSeats$Sales, col ='light blue')

carSeats$Sales

#Visualizing the dataset, we see that there are variation of number of sales
# We also know that the mean of the sales Number is 7.496325
# Now, we are going to take an approach of classifying the data to
# Either High or Low, anything less or equal to 7 is Low and everything else is high
set.seed(33)
mean(carSeats$Sales)

high <- ifelse(carSeats$Sales <= 7, "No", "Yes")

# Next, let's tranform the dataset including the classificiation into a DataFrame()
carseats <- data.frame(carSeats, high)

# Now, we start filling a model into a decision Tree, excluding the Sales variable
decTree <- tree(high ~. -Sales, data =carseats)

# Now let's run a summary() to view our Classification Tree
summary(decTree)

# Next, we can do a plot to see what's going on with our Classification Tree
plot(decTree, col ='red')

# Now, let us anotate it with the text() function
text(decTree, pretty =1)

# Now, let us divide our data into a training set and validation set
# Splitting our dataset by 70%/30% Training/Testing respectively
set.seed(112)
carseats.Ind <- sample(2, nrow(carseats), replace =TRUE, prob =c(0.7, 0.3))
carTrain <- carseats[carseats.Ind==1,]
carTest <- carseats[carseats.Ind==2,]

train <-sample(1:nrow(carseats), 265)
# Now let us create a Tree set using the Training Data
# and including a Subset
treeCars <- tree(high ~. -Sales, carseats, subset=train)
plot(treeCars, col ='orange')
text(treeCars, pretty =1)

# Now that we have our Decision Tree for the Training set, we can use it to run prediction on the validation set
predictCars <- predict(treeCars, carseats[-train,], type ="class")

# Next, we can evaluate the error using the misclassification table from our library imported above
set.seed(101)
with(carseats[-train,], table(predictCars, high))

# The Misclassification Error is:
sprintf("The Misclassification Error is %1.2f%%", ((42 + 58) / 135) *100)


# Now, we are going to use Cross-validation to prune the tree.
(valideCar <- cv.tree(treeCars, FUN =prune.misclass))
plot(valideCar, col ='lime green')

carPrune <- prune.misclass(treeCars, best =10)
plot(carPrune, col ='orange')
text(carPrune, pretty =1)

# Now, we can text this on our training set
predictTree <- predict(carPrune, carseats[-train,], type ="class")
with(carseats[-train,], table(predictTree, high))
# The Misclassification Error is:
sprintf("The Misclassification Error is now %1.2f%%", ((45 + 56) / 135) *100)