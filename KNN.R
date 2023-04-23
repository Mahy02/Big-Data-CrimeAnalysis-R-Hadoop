
#We will use KNN to detect type of crime given => neighborhood, type of location, and the date



#1.First of all, start by cleaning the workspace and setting the working directory.
rm(list=ls())


#setwd("D:/MahyDolphin/College_Stuff/Senior1/2nd_sem/Big_data/project")

#----------------------------------------------------------------------------
#Step 1:
# read the data into a table from the file
#Note that some rows dont have 30 elements so we will use Fill= true to fill missing values with empty strings
#then we will use na.strings="" to tell R to treat any "" as missing value

crime_data <- read.csv("Major_Crime_Indicators.csv",  fill=TRUE, na.strings="", sep=",", header = TRUE)

#---we will filter out to remove any row that has missing values in it using rowSums(is.na(crime_data)) == 0 
#first is.na(crime_data) returns a boolean matrix that has TRUE for any NA value and FALSE for any non-NA value in crime_data.
#The rowSums() function calculates the sum of TRUE values across each row, so a row with no missing values will have a sum of zero. 
#Therefore, rowSums(is.na(crime_data)) == 0 returns a logical vector indicating which rows have no missing values.

#--- we will also filter out any cols that are not=30 "there could be more than 30" so we use  ncol(crime_data) == 30 to ensure only no. of cols=30
#This helps prevents errors later in the dataset and incorrect analysis

#---At the end we are getting a subset of crime_data for ROWS that satisfy this conditions

crime_data <- crime_data[rowSums(is.na(crime_data)) == 0 & ncol(crime_data) == 30, ]
head <- head(crime_data, n=3) 
tail <- tail(crime_data, n=3)
head
tail
str(crime_data)

#-----------------------------------------------------------------------------------
#Step2:  
#subset to the required fields only

#mci_category will be our predicted variable "type of crime"
#we need to narrow down to predictor variables only that will be significant for building our model
#which are: neighborhood, type of location, and the date
#:Neighbourhood, occurrencedate  , location_type
#crime_data_subset <- crime_data[c('mci_category','Neighbourhood','premises_type','occurrencedate', 'X', 'Y')]

crime_data_subset <- crime_data[c('mci_category','Neighbourhood','premises_type','occurrencemonth', 'occurrencedayofweek', 'occurrencehour')]

str(crime_data_subset)
head(crime_data_subset)


#-----------------------------------------------------------------------------------------
#Step 3: Normalization, Standarization and categorial 
library(tidyr)
library(dplyr)


# Step 3: Normalize, standardize and encode data

# One-hot encoding for categorical variables
encoded_data <- crime_data_subset %>% 
  select(-mci_category) %>% 
  mutate_if(is.character, factor) %>% 
  mutate(across(everything(), as.integer))

head(encoded_data)

# Standardization for numerical data
encoded_data$occurrencehour <- scale(encoded_data$occurrencehour)
head(encoded_data)
str(encoded_data)

final_data <- encoded_data
#-------------------------------------------------------------------------------------
#Step 4:
#Split the data: 
#Split the dataset into training and testing sets. 
#The training set will be used to fit the KNN model
#the testing set will be used to evaluate its performance.

#sets the seed for the random number generator to 123. This ensures that the random sampling is reproducible, i.e., every time you run the code with the same seed, you get the same random numbers.
set.seed(123)

#selects a random sample of 70% of the rows from the data frame. 
#1:nrow(final_data) generates a vector of row numbers from 1 to the number of rows in final_data,
#and size=nrow(final_data)*0.7 specifies that we want to select 70% of the rows. 
#replace=FALSE ensures that each row is selected only once.
random_sel <- sample(1:nrow(final_data),size=nrow(final_data) * 0.7,replace = FALSE) #random selection of 70% data.
random_sel

#create a new data frame train_data containing the 70% of the rows selected. 
#The row indices are given by random_sel, and the columns are the same as in the original final_data data frame.
train_data <- final_data[random_sel,] # 70% training data
#creates a new data frame test_data containing the remaining 30% of the rows not selected. The - sign before random_sel means that we exclude the rows with the indices in random_sel
test_data <- final_data[-random_sel,] # remaining 30% test data

head(train_data)
head(test_data)

#---------------------------------------------------------------------------------
#Step 3:  "Static K for now"
#Next, we need to choose the value of K i.e. the nearest data points.
k=5


#-----------------------------------------------------------------------------------
#Step 4: Train the KNN model on the training data
#==> For each point in the test data do the following 
     #✓ Calculate the distance between test data and each row of training data with the  help of any of the method namely: Euclidean, Manhattan or Hamming distance. 
     #✓ Now, based on the distance value, sort them in ascending order. [training]
     #✓ Next, it will choose the top K rows from the sorted array. [closest data to test data]
     #✓ Now, it will assign a class to the test point based on most frequent class of these rows. K=5 , we calculate distance between testdata and all our data. Then sort and choose best closest 5
# we will do all these steps using built in libraries


library(class)
#class lables:
train_mci_category <- crime_data_subset[random_sel,1]
test_mci_category <- crime_data_subset[random_sel,1]
#using Knn function that takes train data, test data and the class labels with k =5
knn_model <- knn(train = train_data, test = test_data, cl = train_mci_category, k = 5)
knn_model
#the output is the predicted class label for test data
str(knn_model)


#step 5: # Evaluate the accuracy of the KNN model on the test data
# calculate accuracy
N <- length(test_mci_category)
accuracy <- 100 * sum(knn_model == test_mci_category) / N

# print accuracy
accuracy




