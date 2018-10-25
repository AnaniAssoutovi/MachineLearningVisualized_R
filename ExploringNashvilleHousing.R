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

# Now Let us begin by importing the dataset that we're going to be analyzing
#data <- read.csv(file.choose(), header =T, sep =",")
data <- read.csv("Nashville_housing_data_2013_2016.csv", header =T, sep =",")

# Let us do a quick look into the dataset to see what's going in there
#Simple analsis shows that:
  # We have:
  # 1) 56,636 Observation of 31 Variables
  # 2) we also know that the majority of our variables are of type Factor and int
  # 3) We can tell that this is trully a dataset based on home prices and demographic
set.seed(33)
head(data)
tail(data)
colnames(data)
class(data)
str(data)

# Let us do some cleaning on the column name so they're not well structured
colnames(data) <- c("X","Unnamed","ParcelID","LandUse","PropertyAddress","SuiteCondo",
                    "PropertyCity","SaleDate","SalePrice","LegalReference","SoldAsVacant",
                    "MultipleParcelsInvolvedInSale","OwnerName","Address","City","State",
                    "Acreage","TaxDistrict","Neighborhood","image","LandValue","BuildingValue",
                    "TotalValue","FinishedArea","FoundationType","YearBuilt","ExteriorWall","Grade",
                    "Bedrooms","FullBath","HalfBath")
colnames(data)
# Next, let us do some cleaning because the data has a lot of NA


# Let us now split the data into training and validation sets
# index<- sample(1:nrow(data),nrow(data)*0.75, replace = F)
# Nash_Train<- data[index,]
# Nash_Test<- data[-index,]
# Nash_Train <- sample(1:nrow(data), 39759)

p <- qplot(HalfBath, YearBuilt, data =data, shape ="z", col ="orange", facets =FoundationType~ExteriorWall,
           main ="Scatterplots of FullBath vs HalfBath of Houses", xlab ="Half Bath", ylab ="Year Built")
p + theme_bw()

# Kernel density plots for Foundation Type grouped by numbers of Full Bath (indicated by colors)
qplot(FoundationType, data =data, geom ="density", fill =FullBath, alpha =I(.5),
      main ="Distribution of Foundation Type", xlab ="Foundation Type", ylab ="Density")

# Kernel density plots for Foundation Type grouped by numbers of Half Bath (indicated by colors)
qplot(FoundationType, data =data, geom ="density", fill =HalfBath, alpha =I(.5),
      main ="Distribution of Foundation Type", xlab ="Foundation Type", ylab ="Density")

# Scatterplot of Full Bath vs. Half Bath for each combination of Foundation Type and Bedrooms
# In each facet, Foundation Type is represented by shape and color
qplot(HalfBath, FullBath, data =data, shape ="z", color ='orange', facets =FoundationType~Bedrooms,
      size =I(3), xlab ="Half Bath", ylab ="Full Bath")

#Let us now start Fitting the Random Forest
Nash_House_Type <- randomForest(YearBuilt ~ ExteriorWall+FoundationType, data =data, na.action =na.omit)
plot(Nash_House_Type, col ='orange')

Nash_House_District <- randomForest(YearBuilt ~ LandValue+BuildingValue+City+TaxDistrict, data =data, na.action =na.omit)
plot(Nash_House_District, col ='orange')

