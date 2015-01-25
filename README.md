# Getting-and-Cleaning-Data-Project

# Load packages for analysis
library(dplyr)
library(tidyr)

# Download raw data 
raw <- download.file("http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",destfile = "project.zip")

# Unzip data 
unzip('project.zip')

# Read data from test and train data sets into R
subject_test <- read.table("~/RWorkspace/UCI HAR Dataset/test/subject_test.txt", quote="\"")
X_test <- read.table("~/RWorkspace/UCI HAR Dataset/test/X_test.txt", quote="\"")
Y_test <- read.table("~/RWorkspace/UCI HAR Dataset/test/y_test.txt", quote="\"")
subject_train <- read.table("~/RWorkspace/UCI HAR Dataset/train/subject_train.txt", quote="\"")
X_train <- read.table("~/RWorkspace/UCI HAR Dataset/train/X_train.txt", quote="\"")
Y_train <- read.table("~/RWorkspace/UCI HAR Dataset/train/y_train.txt", quote="\"")

#Merge test data set
colnames(subject_test) <- "subject"
colnames(Y_test) <- "activity"
test <- cbind(subject_test,Y_test,X_test)

#Merge train data set
colnames(subject_train) <- "subject"
colnames(Y_train) <- "activity"
train <- cbind(subject_train,Y_train,X_train)

#Merge test data and train data into one
total <- rbind(test,train)

#Extract only the measurements on the mean for each measurement.

features <- read.table("~/RWorkspace/UCI HAR Dataset/features.txt", quote="\"",stringsAsFactors=F)

meanID <- filter(features,grepl("mean",V2))[,1]
totalMean <- total %>% select(subject, activity,meanID+2)

#Extract only the measurements on the mean for each measurement.
stdID <- filter(features,grepl("std",V2))[,1]

totalStd <- total %>% select(subject, activity,stdID+2)

# Uses descriptive activity labels to name the activities in each data set
activity_labels <- read.table("~/RWorkspace/UCI HAR Dataset/activity_labels.txt", quote="\"", stringsAsFactors=FALSE)
colnames(activity_labels) <-c("activity", "activity_labels")

totalMean <- merge(x=activity_labels,y=totalMean,by="activity")
totalStd <- merge(x=activity_labels,y=totalStd,by="activity")

# Label the data set with descriptive variable names. 
names(totalMean)[4:49]<- filter(features,V1 %in% meanID)[,2]
names(totalStd)[4:36]<- filter(features,V1 %in% stdID)[,2]

# Create a second, independent tidy data set with the average of each variable for each activity and each subject

averageMean <- totalMean %>% gather(variable,value, -c(activity_labels:subject)) %>% group_by(activity_labels, subject, variable) %>% summarize(mean = mean(value))

averageStd <- totalStd %>% gather(variable,value, -c(activity_labels:subject)) %>% group_by(activity_labels, subject, variable) %>% summarize(mean = mean(value))

averageAll <- rbind(averageMean,averageStd)                                                                                                                                              
# Output averageAll into text file to get the tidydata test file.
write.table(averageAll,file = "tidydata.txt",row.names = FALSE)


