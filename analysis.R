### Setting Working Directory
workingDirectory <- "C:/Users/ahmet.kalafat/Desktop/Coursera/Getting And Cleaning 
setwd("C:/Users/ahmet.kalafat/Desktop/Coursera/Getting And Cleaning Data")

### Creating the sub directory for the data
if(!file.exists("data")){dir.create("data")}

### Downloading Data and Opening Zip File

zipFileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(zipFileUrl, destfile ="data/dataSet.zip")

### Unzip dataSet to /data directory
unzip(zipfile="./data/Dataset.zip",exdir="./data")

### Load required packages

library(dplyr)
library(data.table)
library(tidyr)

### Reading Data From These Files

featureNames <- read.table("data/UCI HAR Dataset/features.txt")
activityLabels <- read.table("data/UCI HAR Dataset/activity_labels.txt", header = FALSE)
subjectTrain <- read.table("data/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("data/UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("data/UCI HAR Dataset/train/X_train.txt", header = FALSE)
subjectTest <- read.table("data/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("data/UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("data/UCI HAR Dataset/test/X_test.txt", header = FALSE)

### 1 - Merges the training and the test sets to create one data set.
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)
colnames(features) <- t(featureNames[2])
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features, activity, subject)

### Extracts only the measurements on the mean and standard deviation for each measurement
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)
requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)

extractedData <- completeData[,requiredColumns]
dim(extractedData)

extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
extractedData$Activity[extractedData$Activity == i] <- as.character

(activityLabels[i,2])
}
extractedData$Activity <- as.factor(extractedData$Activity)

names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

### 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "TidyData.txt", row.names = FALSE)
