# Author: Anani Assoutovi
# @Tag: יְהִי רָצוֹן מִלְּפָנֶֽיךָּ
# Date: November 3rd 2018
# Time: 23:12:12

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
libs <- c("caret", "e1071")
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
    setWork <- setwd("~/Desktop/MachineLearningVisualized_R/PredictiveAnalysis/")
  }
  else{
    print("I DO NOT RECOGNIZED YOUR OS!!!")
  }
}
setWorkDir()
getwd()

# Loading the dataset
Data <- read.csv("tictactoe.csv", head =F, sep =",", stringsAsFactors =T)

# Let us do a quick visual analysis of the dataset
head(Data)

# Right of the back, we see something omniuous with the colnames
# I know what you're thinking, we set the head to FALSE as we did the import
# The reason is because the original colnames are funky, we don't want that
# Now, let us change the colnames of the dataset to be something more meaningful
# The dataset did not come with anymore information, sorry. Bare with me.

# Changing the Colnames() of the dataset
colnames(Data) <- c("tls", "tms", "trs", "mls", "mms", "mrs", "bls", "bms", "brs", "class")

# Now let us view it again.
head(Data)
str(Data)

# Because the variables types are in Factor(), it makes us hard for us to do SVN analysis
# So therefore let us consider changing some of the variable types to numeric
# By using as.numeric() function on the variables

# Cleaning the Dataset by converting Variables 1:9 to numeric
Data$tls <- as.numeric(Data$tls)
Data$tms <- as.numeric(Data$tms)
Data$trs <- as.numeric(Data$trs)
Data$mls <- as.numeric(Data$mls)
Data$mms <- as.numeric(Data$mms)
Data$mrs <- as.numeric(Data$mrs)
Data$bls <- as.numeric(Data$bls)
Data$bms <- as.numeric(Data$bms)
Data$brs <- as.numeric(Data$brs)

str(Data)


# Split into Train and Validation sets
# Training Set: Validation Set = 70 : 30 (Random)
train <- sample(nrow(Data), 0.7 *nrow(Data), replace =T)
TrainSet <- Data[train,]
ValidSet <- Data[-train,]
summary(TrainSet)
summary(ValidSet)

# Now, let us perform some SVN modeling on the dataset
set.seed(1234)
(a <- Data$class)
(b <- subset(Data, select =- class))

(mod <- svm(class ~., data =Data))

# Let us do some predictions now
(pred <- predict(mod, b))

# Let us do a little plot to visual what's going on here
plot(pred, col ='orange')

# Now let us throw the prediction and the predictor into a confusionMatrix
confusionMatrix(pred, a)

# Next, for us to get an accurate result of the prediction, we can run a cross validation fold
CV_Folds <- createMultiFolds(a, k =10, times =10)

tRControl <- trainControl(method ='repeatedCV', index =CV_Folds)


trainFunction <- function(methodType){
  return(train(b, a, method = methodType, tuneLength =5, trControl = tRControl))
}

# To Train SVM for a Polynomial Kernel
# To Train SVM for a linear Kernel
# To Train SVM for a Radial Kernel

# I strongly do not recommend using the method below, 1: you machine would crash 2: if it doesn't crash you'd be waiting a lifetime
# Bon Apetit

kernelToUse <- c("svmPoly","svmLinear","svmRadial")
for(i in kernelToUse){
  KernModel <- trainFunction(i)
  print(KernModel)
  print(predict(KernModel, b))
}

# Instead of going with the method above, let us try another method
funcTrain <- function(kernel){
  KernModel <- trainFunction(kernel)
  print(KernModel)
  print(predict(KernModel, b))
}

funcTrain("svmPoly")

funcTrain("svmLinear")

funcTrain("svmRadial")

