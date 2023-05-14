#We will use KNN to detect type of crime given => neighborhood, type of location, and the date



#1.First of all, start by cleaning the workspace and setting the working directory.
rm(list=ls())


#setwd("D:/MahyDolphin/College_Stuff/Senior1/2nd_sem/Big_data/project")

setwd("F:/Senior-2-Spring/Big-Data/Project-Repo/Big-Data-CrimeAnalysis-R-Hadoop") 

#setwd("D:/MahyDolphin/College_Stuff/Senior1/2nd_sem/Big_data/project")
#----------------------------------------------------------------------------
#Step 1:
# read the data into a table from the file
#Note that some rows dont have 30 elements so we will use Fill= true to fill missing values with empty strings
#then we will use na.strings="" to tell R to treat any "" as missing value

crime_data <- read.csv("Major_Crime_Indicators.csv")
anyNA(crime_data$mci_category)
anyNA(crime_data$reportedday)
anyNA(crime_data$reportedyear)
anyNA(crime_data$reportedmonth)
anyNA(crime_data$reportedhour)
anyNA(crime_data$offence)
anyNA(crime_data$Neighbourhood)
anyNA(crime_data$premises_type)
anyNA(crime_data$occurrenceday)
anyNA(crime_data$occurrencehour)
anyNA(crime_data$occurrencemonth)
anyNA(crime_data$occurrenceyear)
anyNA(crime_data$occurrencedate)

#=========================Filling missing value====================================
date_year<-c(crime_data$occurrencedate[is.na(crime_data$occurrenceyear)])
#filling missing data in occurrence year from the column occurrence date
crime_data$occurrenceyear[is.na(crime_data$occurrenceyear)]<-substr(date_year,1,4)
#checking that they are all filled
anyNA(crime_data$occurrenceyear)

date_day<-c(crime_data$occurrencedate[is.na(crime_data$occurrenceday)])
#filling missing data in occurrence day from the column occurrence date
crime_data$occurrenceday[is.na(crime_data$occurrenceday)]<-substr(date_day,9,10)
#checking that they are all filled
anyNA(crime_data$occurrenceday)



date_month<-c(crime_data$occurrencedate[crime_data$occurrencemonth==""])

#filling missing data in occurrence day from the column occurrence date
crime_data$occurrencemonth[crime_data$occurrencemonth==""]<-substr(date_month,6,7)
levels(factor(crime_data$occurrencemonth))



#=================Finding corelation between features================================
#we have to convert categorial data to numerical data
crime_data$reportedmonth
crime_data$reportedmonth <- unclass(crime_data$reportedmonth)
crime_data$reportedmonth
crime_data$reportedmonth[which(crime_data$reportedmonth=="January")] <- "1"
crime_data$reportedmonth[which(crime_data$reportedmonth=="February")] <- "2"
crime_data$reportedmonth[which(crime_data$reportedmonth=="March")] <- "3"
crime_data$reportedmonth[which(crime_data$reportedmonth=="April")] <- "4"
crime_data$reportedmonth[which(crime_data$reportedmonth=="May")] <- "5"
crime_data$reportedmonth[which(crime_data$reportedmonth=="June")] <- "6"
crime_data$reportedmonth[which(crime_data$reportedmonth=="July")] <- "7"
crime_data$reportedmonth[which(crime_data$reportedmonth=="August")] <- "8"
crime_data$reportedmonth[which(crime_data$reportedmonth=="September")] <- "9"
crime_data$reportedmonth[which(crime_data$reportedmonth=="October")] <- "10"
crime_data$reportedmonth[which(crime_data$reportedmonth=="November")] <- "11"
crime_data$reportedmonth[which(crime_data$reportedmonth=="December")] <- "12"
crime_data$reportedmonth
crime_data$reportedmonth <- as.numeric(crime_data$reportedmonth)
crime_data$reportedmonth

crime_data$occurrencemonth[which(crime_data$occurrencemonth=="January")] <- "1"
crime_data$occurrencemonth[which(crime_data$occurrencemonth=="February")] <- "2"
crime_data$occurrencemonth[which(crime_data$occurrencemonth=="March")] <- "3"
crime_data$occurrencemonth[which(crime_data$occurrencemonth=="April")] <- "4"
crime_data$occurrencemonth[which(crime_data$occurrencemonth=="May")] <- "5"
crime_data$occurrencemonth[which(crime_data$occurrencemonth=="June")] <- "6"
crime_data$occurrencemonth[which(crime_data$occurrencemonth=="July")] <- "7"
crime_data$occurrencemonth[which(crime_data$occurrencemonth=="August")] <- "8"
crime_data$occurrencemonth[which(crime_data$occurrencemonth=="September")] <- "9"
crime_data$occurrencemonth[which(crime_data$occurrencemonth=="October")] <- "10"
crime_data$occurrencemonth[which(crime_data$occurrencemonth=="November")] <- "11"
crime_data$occurrencemonth[which(crime_data$occurrencemonth=="December")] <- "12"
crime_data$occurrencemonth <- as.numeric(crime_data$occurrencemonth)


week_day<-c(crime_data$occurrencedate[crime_data$occurrencedayofweek == ""])
week_day
crime_data$occurrencedayofweek[(crime_data$occurrencedayofweek == "")] <-strftime(substr(week_day,1,10), format = "%A")
crime_data$occurrencedayofweek[(crime_data$occurrencedayofweek == "Friday    ")]<-"Friday"
crime_data$occurrencedayofweek[(crime_data$occurrencedayofweek == "Monday    ")]<-"Monday"
crime_data$occurrencedayofweek[(crime_data$occurrencedayofweek == "Saturday  ")]<-"Saturday"
crime_data$occurrencedayofweek[(crime_data$occurrencedayofweek == "Sunday    ")]<-"Sunday"
crime_data$occurrencedayofweek[(crime_data$occurrencedayofweek == "Thursday  ")]<-"Thursday"
crime_data$occurrencedayofweek[(crime_data$occurrencedayofweek == "Tuesday   ")]<-"Tuesday"
crime_data$occurrencedayofweek[(crime_data$occurrencedayofweek == "Wednesday ")]<-"Wednesday"
levels(factor(crime_data$occurrencedayofweek))



crime_data$occurrencedayofweek[which(crime_data$occurrencedayofweek=="Sunday")] <- "1"
crime_data$occurrencedayofweek[which(crime_data$occurrencedayofweek=="Monday")] <- "2"
crime_data$occurrencedayofweek[which(crime_data$occurrencedayofweek=="Tuesday")] <- "3"
crime_data$occurrencedayofweek[which(crime_data$occurrencedayofweek=="Wednesday")] <- "4"
crime_data$occurrencedayofweek[which(crime_data$occurrencedayofweek=="Thursday")] <- "5"
crime_data$occurrencedayofweek[which(crime_data$occurrencedayofweek=="Friday")] <- "6"
crime_data$occurrencedayofweek[which(crime_data$occurrencedayofweek=="Saturday")] <- "7"
crime_data$occurrencedayofweek <- as.numeric(crime_data$occurrencedayofweek)




#-----------------------------------------------------------------------------------
#Step2:  
#subset to the required fields only

#mci_category will be our predicted variable "type of crime"
#we need to narrow down to predictor variables only that will be significant for building our model
#which are: 'location_type','Neighbourhood','premises_type','occurrencemonth', 'occurrencedayofweek', 'occurrencehour'


crime_data_subset <- crime_data[c('location_type','Neighbourhood','premises_type','occurrencemonth', 'occurrencedayofweek', 'occurrencehour')]
crime_data_subset$location_type<-unclass(factor(crime_data_subset$location_type))
crime_data_subset$location_type
crime_data_subset$Neighbourhood= unclass(factor(crime_data_subset$Neighbourhood))
crime_data_subset$premises_type=unclass(factor(crime_data_subset$premises_type))

levels(factor(crime_data$offence))
str(crime_data_subset)
head(crime_data_subset)


#-----------------------------------------------------------------------------------------
#Step 3: Normalization, Standarization and categorial 

#We don't need normalization or standarization as we converted categorial data to numerical so all data is within the same range



final_data <- crime_data_subset
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
#and size=nrow(final_data)*0offence reportedyear reportedmonth reportedday reporteddayofyear reporteddayofweek
#1     1430     100       6   .7 specifies that we want to select 70% of the rows. 
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
k=3


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
train_mci_category <- crime_data[random_sel,25]

test_mci_category <- crime_data[-random_sel,25]
# different values of K produced different accuracies 
# K=17 produced the best accuracy and trying K > 17 didn't produce any significant effect on the accuracy
#30 60.2%
#25 60.2%
#17 60%
#13 59.7%
#9 59.4%
#7 59.2%
#5 58%
#using Knn function that takes train data, test data and the class labels with k =5
knn_model <- knn(train = train_data, test = test_data, cl = train_mci_category, k = 17)
knn_model
#the output is the predicted class label for test data
str(knn_model)


#step 5: # Evaluate the accuracy of the KNN model on the test data
# calculate accuracy
N <- length(test_mci_category)
accuracy <- 100 * sum(knn_model == test_mci_category) / N

# print accuracy
accuracy






#---------------------------------------------------------------------------------------------

crime_data_subset2 <- crime_data[c('location_type','mci_category','Neighbourhood','offence','occurrencemonth', 'occurrencedayofweek', 'occurrencehour')]
crime_data_subset2$location_type<-unclass(factor(crime_data_subset2$location_type))
crime_data_subset2$location_type
crime_data_subset2$mci_category= unclass(factor(crime_data_subset2$mci_category))
crime_data_subset2$Neighbourhood=unclass(factor(crime_data_subset2$Neighbourhood))
crime_data_subset2$offence=unclass(factor(crime_data_subset2$offence))


final_data2 <- crime_data_subset2
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
#and size=nrow(final_data)*0offence reportedyear reportedmonth reportedday reporteddayofyear reporteddayofweek
#1     1430     100       6   .7 specifies that we want to select 70% of the rows. 
#replace=FALSE ensures that each row is selected only once.
random_sel2 <- sample(1:nrow(final_data2),size=nrow(final_data2) * 0.7,replace = FALSE) #random selection of 70% data.
random_sel2

#create a new data frame train_data containing the 70% of the rows selected. 
#The row indices are given by random_sel, and the columns are the same as in the original final_data data frame.
train_data2 <- final_data2[random_sel2,] # 70% training data
#creates a new data frame test_data containing the remaining 30% of the rows not selected. The - sign before random_sel means that we exclude the rows with the indices in random_sel
test_data2 <- final_data2[-random_sel2,] # remaining 30% test data


#---------------------------------------------------------------------------------
#Step 3:  "Static K for now"
#Next, we need to choose the value of K i.e. the nearest data points.
k=3


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
train_neighborhood2 <- crime_data[random_sel2,9]

test_neighborhood2 <- crime_data[-random_sel2,9]
# different values of K produced different accuracies 
# K=17 produced the best accuracy and trying K > 17 didn't produce any significant effect on the accuracy
#30 60.2%
#25 60.2%
#17 60%
#13 59.7%
#9 59.4%
#7 59.2%
#5 58%
#using Knn function that takes train data, test data and the class labels with k =5
knn_model2 <- knn(train = train_data2, test = test_data2, cl = train_neighborhood2, k = 17)
knn_model2
#the output is the predicted class label for test data



#step 5: # Evaluate the accuracy of the KNN model on the test data
# calculate accuracy
N2 <- length(test_neighborhood2)
accuracy2 <- 100 * sum(knn_model2 == test_neighborhood2) / N2

# print accuracy
accuracy2